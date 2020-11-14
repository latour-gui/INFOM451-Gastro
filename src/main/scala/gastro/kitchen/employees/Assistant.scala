package gastro.kitchen.employees

import akka.actor.Actor
import gastro.kitchen._
import gastro.kitchen.food.Product

class Assistant extends Actor {
  override def receive: Receive = {
    case CoreProductMessage =>
      sender ! CoreProductResponse(findCoreItem())

    case QuantityMessage(m) =>
      sender ! QuantityResponse(calculateQuantities(m))

    case _ => println("Assistant received a incomprehensible message")
  }

  def findCoreItem(): Product =
    new Product(1, "skeleton", Nil)


  def calculateQuantities(menu: Menu): String = "whaaaaaat"
}
