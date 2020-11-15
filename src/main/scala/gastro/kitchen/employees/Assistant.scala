package gastro.kitchen.employees

import akka.actor.{Actor, ActorRef}
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food._

import scala.util.{Failure, Random, Success}

class Assistant extends Actor {
  override def receive: Receive = {
    case CoreProductMessage =>
      sender ! CoreProductResponse(findCoreItem())

    case QuantityMessage(m) => calculateAndSendQuantities(m, sender())

    case _ => promptMessage("Assistant received a incomprehensible message")
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


  def calculateAndSendQuantities(menu: Menu, coq: ActorRef): Unit = {
    val portions: Seq[(Integer, String, String)] = lookForPortions() match {
      case Success(value) => value
      case Failure(exception) =>
        promptMessage(exception.getMessage)
        Nil
    }

    val quantifiedProducts = menu.products.map(p => {
      val proportionTuple: (Integer, String, String) = portions.find(_._1 == p.id).getOrElse((p.id, "Quantity not specified", "0"))
      QuantifiedProduct(p, proportionTuple._2, proportionTuple._3)
    })


    coq ! QuantityResponse(QuantifiedMenu(quantifiedProducts))
  }
}