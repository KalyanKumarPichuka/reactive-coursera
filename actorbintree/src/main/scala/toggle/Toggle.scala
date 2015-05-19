package toggle

import akka.actor.Actor
import akka.event.LoggingReceive

/**
 * Created by tomas on 19/05/15.
 */
class Toggle extends Actor {

  def happy: Receive = LoggingReceive {
    case "How are you?" => sender ! "happy"; context become sad
  }

  def sad = LoggingReceive {
    case "How are you?" => sender ! "sad"
      context become(happy)
  }

  def receive = LoggingReceive { happy }
}
