package bank

import akka.actor.{Props, Actor}
import akka.event.LoggingReceive
import bank.BankAccount.Deposit

/**
 * Created by tomas on 19/05/15.
 */
class Main extends Actor {

  val accountA = context.actorOf(Props[BankAccountActor], "accountA")
  val accountB = context.actorOf(Props[BankAccountActor], "accountB")

  accountA ! Deposit(100)

  def receive = {
    case BankAccount.Done => self ! transfer(1150)
  }

  def transfer(amount: BigInt) = {
    val transfer = context.actorOf(Props[TransferActor], "transfer")
    transfer ! WireTransfer.Transfer(accountA, accountB, amount)
    context.become(LoggingReceive {
      case WireTransfer.Done =>
        println("transfer successful")
        context.stop(self)
      case WireTransfer.Failed =>
        println(s"Cannot transfer amount to $accountA")
        context.stop(self)
    })
  }

}
