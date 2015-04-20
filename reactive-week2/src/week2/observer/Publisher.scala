package week2.observer

trait Publisher {

  private var subscribers: Set[Subscriber] = Set()

  def subscribe(s: Subscriber) =
    subscribers += s

  def unsubscribe(s: Subscriber) =
    subscribers -= s

  def publish =
    subscribers foreach(_.handle(this))
}

trait Subscriber {
  def handle(p: Publisher): Unit
}
