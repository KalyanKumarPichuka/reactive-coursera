/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import akka.event.LoggingReceive
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => root ! op
    case GC            => val nowRoot = createRoot
                          root ! CopyTo(nowRoot)
                          context.become(garbageCollecting(nowRoot))
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case GC => None
    case op: Operation => pendingQueue = pendingQueue.enqueue(op)
    case CopyFinished => {
      // Use PoisonPill to finish
      root ! PoisonPill
      root = newRoot

      // Dequeue and attend messages
      while(!pendingQueue.isEmpty) {
        val (message, lighterQueue) = pendingQueue.dequeue
        pendingQueue = lighterQueue
        root ! message
      }
      pendingQueue = Queue.empty[Operation]
      context.become(normal)
    }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    // Handle insert for equal, less and greater values
    case ins @ Insert(rq, id, elem) =>
      this.elem compareTo elem match {
        case n if n == 0 => this.removed = false
        case n if n > 0 => subtrees get Right match {
            // If I get some address, I need to insert the element, that guy must insert the elem
          case Some(actorRef) => actorRef ! ins
            // Insert in my subtrees as a tuple (Right, Node#elem)
          case _           =>
            subtrees += Right -> context.actorOf(props(elem, false), s"Node$elem")
        }
          // this.elem < elem
        case _ => subtrees get Left match {
          case Some(actorRef) => actorRef ! Insert(rq, id, elem)
          case _              =>
            subtrees += Left -> context.actorOf(props(elem, false), s"Node$elem")
        }
      }
      rq ! OperationFinished(id)
      // Handle remove
    case rm @ Remove(rq, id, elem) =>
      this.elem compareTo elem match {
        case n if n == 0 => this.removed = true
        case n if n > 0  => subtrees get Left match {
          case Some(actorRef) => actorRef ! rm
          case _              => sender ! OperationFinished(id)
        }
        case _                => subtrees get Right match {
          case Some(actorRef) => actorRef ! rm
          case _              => sender ! OperationFinished(id)
        }
      }
    case CopyTo(treeNode) => {
      if (subtrees.isEmpty && removed) context.parent ! CopyFinished
      else {
        val actorRefs = subtrees.values.toSet
        context.become(copying(actorRefs, removed))

        actorRefs foreach (_ ! CopyTo(treeNode))
        if (!removed) treeNode ! Insert(self, -1, elem)
      }
    }
    case cont @ Contains(rq, id, elem) =>
      this.elem compareTo elem match {
        case n if n == 0 => context.parent ! ContainsResult(id, !removed)
        case n if n > 0  => subtrees get Left match {
          case Some(actorRef) => actorRef ! cont
          case _              => context.parent ! ContainsResult(id, false)
        }
        case _                => subtrees get Right match {
          case Some(actorRef) => actorRef ! cont
          case _              => context.parent ! ContainsResult(id, false)
        }
      }
    case ContainsResult(id, result) => context.parent ! ContainsResult(id, result)
    case OperationFinished(id) => context.parent ! OperationFinished(id)
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = LoggingReceive {
    case OperationFinished(id) =>
      if (expected.isEmpty) context.parent ! CopyFinished
      else context.become(copying(expected, true))
    case CopyFinished =>
      val newRefs = expected - sender
      if (newRefs.isEmpty && insertConfirmed) context.parent ! CopyFinished
      else context.become(copying(newRefs, insertConfirmed))
  }


}
