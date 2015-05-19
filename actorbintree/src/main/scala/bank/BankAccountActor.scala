package bank

import akka.actor.{ActorRef, Actor}
import bank.BankAccount.{Deposit, Withdraw}
import akka.event.LoggingReceive

/**
 * Created by tomas on 18/05/15.
 */
// If a message enters while another one is being processed, it is queued
class BankAccountActor extends Actor{
  import BankAccount._

  var balance = BigInt(0)

  def receive = LoggingReceive {
    case Deposit(x) =>
      balance += x
      sender ! Done
    case Withdraw(x) if balance >= x =>
      balance -= x
      sender ! Done
    case _ => sender ! Failed
  }
}

class TransferActor extends Actor {
  import bank.WireTransfer._

  def receive: Receive = LoggingReceive {
    case Transfer(from, to, amount) =>
      from ! Withdraw(amount)
      // Await the result of withdraw, suspending this execution
      // Virtually "shuts down" without consuming CPU nor memory.
      context.become(awaitWithdraw(to, amount, sender))
  }

  // When withdraw is finished, follow with this
  def awaitWithdraw(to: ActorRef, amount: BigInt, client: ActorRef) = LoggingReceive {
    case BankAccount.Done =>
      to ! Deposit(amount)
      // Await for deposit and tell client
      context.become(awaitDeposit(client))
    case BankAccount.Failed =>
      client ! Failed
      context.stop(self)
  }

  // Confirm that the transaction has been successful
  def awaitDeposit(client: ActorRef) = LoggingReceive {
    case BankAccount.Done =>
      client ! Done
      context.stop(self)
  }
}
