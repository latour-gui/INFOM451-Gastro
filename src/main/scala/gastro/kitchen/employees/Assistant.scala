package gastro.kitchen.employees

import akka.actor.Actor
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food.{Product, lookForProducts, rememberAJR}

import scala.util.{Failure, Random, Success}

class Assistant extends Actor {
  override def receive: Receive = {
    case CoreProductMessage =>
      sender ! CoreProductResponse(findCoreItem())

    case QuantityMessage(m) =>
      sender ! QuantityResponse(calculateQuantities(m))

    case _ => println("Assistant received a incomprehensible message")
  }

  def findCoreItem(): Option[Product] = {
    val random = new Random
    (for {
      ajrs <- rememberAJR()
      products <- lookForProducts(ajrs)
    } yield products) match {
      case Success(p) => Some(p(random.nextInt(p.length)))
      case Failure(exception) =>
        promptMessage(exception.getMessage)
        None
    }
  }


  def calculateQuantities(menu: Menu): String = "whaaaaaat"
}
