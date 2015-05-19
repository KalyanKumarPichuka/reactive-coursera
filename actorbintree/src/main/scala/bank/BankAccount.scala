package bank

import akka.actor.ActorRef

/**
 * Created by tomas on 18/05/15.
 */
// Messages
object BankAccount {
  case class Deposit(amount: BigInt) {
    require(amount > 0)
  }

  case class Withdraw(amount: BigInt){
    require(amount > 0)
  }

  case object Done
  case object Failed
}

object WireTransfer{
  case class Transfer(from: ActorRef, to: ActorRef, amount: BigInt)

  case object Done
  case object Failed
}
