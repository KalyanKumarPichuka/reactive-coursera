package toggle

import akka.actor.{Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import scala.concurrent.duration._

/**
 * Created by tomas on 19/05/15.
 */
class ToggleTest {

  implicit val system = ActorSystem("TestSys")
  val d = new TestKit(ActorSystem("TestSys")) with ImplicitSender {
    val toggle = system.actorOf(Props[Toggle])

    toggle ! "How are you?"
    expectMsg("happy")
    toggle ! "How are you?"
    expectMsg("sad")
    toggle ! "jeje"
    expectNoMsg(1 second)
    // Threads will keep running
    system shutdown
  }

}
