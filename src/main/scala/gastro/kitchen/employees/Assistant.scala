package gastro.kitchen.employees

import akka.actor.{Actor, ActorRef}
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food._

import scala.util.{Failure, Random, Success}

/**
 * The Assistant is responsible for finding the first (core) product
 * He is also responsible for giving the quantities of the composed menu
 *
 * He only interacts with the Coq
 */
class Assistant extends Actor {
  override def receive: Receive = {
    case CoreProductMessage =>
      sender ! CoreProductResponse(findCoreItem())

    case QuantityMessage(m) => calculateAndSendQuantities(m, sender())

    case _ => promptMessage("Assistant received a incomprehensible message")
  }

  // objective : usage of the Option[T] in a function signature
  /**
   * Simply choose a random product in the product list
   *
   * @return random product; None if something fails during the fetching of products.
   */
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

  /**
   * Use the portions csv file to get information concerning quantities of the products in the given menu
   * This function will send the result of its calculation to the Coq
   *
   * @param menu the menu for which we want the product's quantities
   * @param coq  the coq actor's ref
   */
  def calculateAndSendQuantities(menu: Menu, coq: ActorRef): Unit = {
    // objective : treating a function whose signature is of type Try[T]
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