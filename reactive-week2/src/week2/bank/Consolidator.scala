package week2.bank

import week2.observer.{Publisher, Subscriber}


/**
 * Consolidator has the responsibility of keeping
 * the total updated whenever one of the Bank Accounts
 * being observed publishes.
 * @param observed BankAccounts to observe
 */
class Consolidator(observed: List[BankAccount]) extends Subscriber {
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  def currentTotal = total
  compute

  def compute =
    total = observed.map(_.currentBalance).sum

  def handle(p: Publisher) = compute
}
