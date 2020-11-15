package gastro.kitchen.employees

import akka.actor.{Actor, ActorRef, Props}
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food._
import gastro.utils.sanitizeActorName

import scala.util.{Failure, Success}

class SpecialistManager() extends Actor {
  var specialistRefList: Seq[ActorRef] = Nil
  var productList: Seq[Product] = Nil
  var ajrsList: Seq[ajr.Limit] = Nil
  var specificSpecialistResponseCount: Integer = 0
  var bestProductSoFar: Option[(Double, Product)] = None

  /**
   * Small memoization of parameter productList
   *
   * @return the list of product
   */
  def getProductList: Seq[Product] = {
    if (this.productList.isEmpty) {
      // objective : manipulate double call with a Try[T] signature
      this.productList = rememberAJR().map(ajrs => lookForProducts(ajrs)) match {
        case Success(Success(products)) => products
        // since the failure of the nested call would have been propagated,
        // it makes no sense to check for case Failure(Failure(e))
        case Failure(exception) => promptMessage(exception.getMessage); Nil
      }
    }
    this.productList
  }

  /**
   * Small memoization of parameter specialistList
   *
   * @return the list of specialists
   */
  def getSpecialistRefList: Seq[ActorRef] = {
    if (this.specialistRefList.isEmpty) {
      // objective : manipulate method with a Try[T] signature
      this.specialistRefList = rememberAJR().map(_.map(a => context.actorOf(Props(new Specialist(a.name, a.id)), sanitizeActorName(a.name)))) match {
        case Success(sl) => sl
        case Failure(exception) => promptMessage(exception.getMessage); Nil
      }
    }
    this.specialistRefList
  }

  override def receive: Receive = {
    case OtherIngredientMessage(m) => for (s <- this.getSpecialistRefList) {
      s ! BestProductMessage(m, this.getProductList)
    }

    case BestProductResponse(r) => manageSpecificSpecialistResponse(r, sender)

    case _ => promptMessage("Specialist received an incomprehensible message")
  }

  def manageSpecificSpecialistResponse(response: Option[(Double, Product)], assistant: ActorRef): Unit = {
    this.specificSpecialistResponseCount += 1

    response match {
      case Some(r: (Double, Product)) =>
        if (this.bestProductSoFar.isEmpty || r._1 > this.bestProductSoFar.get._1) {
          this.bestProductSoFar = Some(r)
        }
      case None =>
    }

    if (this.specificSpecialistResponseCount == this.getSpecialistRefList.length) {
      //      this.specificSpecialistResponseCount = 0
      //      for (s <- this.getSpecialistRefList) {
      //        context.stop(s)
      //      }
      assistant ! this.bestProductSoFar
    }
  }

}

class Specialist(val name: String, val code: Int) extends Actor {

  def provideBestProduct(menu: Menu, products: Seq[Product]): Option[(Double, Product)] = {
    val choices = products.filter(p => !menu.products.contains(p))

    if (choices.isEmpty) {
      return None
    }

    menu.totalProperties.find(_.id == this.code).map(productProperty => {
      val interestingValue = productProperty.value.normalizedValue
      (for {
        p <- choices
        property <- p.getProperties
        if property.id == this.code
        v = property.value.normalizedValue
      } yield (v / (interestingValue + v), p)).maxBy(_._1)
    })
  }

  override def receive: Receive = {
    case BestProductMessage(menu, products) => sender ! BestProductResponse(provideBestProduct(menu, products))
  }
}
