package gastro.kitchen.employees

import akka.actor.{Actor, ActorRef, Props}
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food._
import gastro.utils.sanitizeActorName

import scala.util.{Failure, Success}

class SpecialistManager() extends Actor {
  var coq: Option[ActorRef] = None
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
        case _ => promptMessage("Prevent warning : now the match is exhaustive"); Nil
      }
    }
    this.productList
  }

  /**
   * Small memoization of parameter specialistList
   *
   * The instantiation of the Specialist actors have to be inside the Props function.
   * The name of the Actors in the `actorOf` function MUST respect some specifications (invalid chars) so a call to
   * `sanitizeActorName` is mandatory
   *
   * @return the list of specialists
   */
  def getSpecialistRefList: Seq[ActorRef] = {
    if (this.specialistRefList.isEmpty) {
      // objective : (bonus) generate child actors on the fly
      // objective : manipulate method with a Try[T] signature
      this.specialistRefList = rememberAJR().map(_.map(a => context.actorOf(Props(new Specialist(a.name, a.id)), sanitizeActorName(a.name)))) match {
        case Success(sl) => sl
        case Failure(exception) => promptMessage(exception.getMessage); Nil
      }
    }
    this.specialistRefList
  }


  override def receive: Receive = {
    case OtherIngredientMessage(m) =>
      this.coq = Some(sender())
      for (s <- this.getSpecialistRefList) {
        s ! BestProductMessage(m, this.getProductList)
      }

    case BestProductResponse(r) => manageSpecificSpecialistResponse(r)

    case _ => promptMessage("Specialist received an incomprehensible message")
  }

  /**
   * Manage the specialists response
   *
   * There is a counter that has been initialised to zero that will certify that all specialists have sent a response
   * There is a memorized tuple (score, product) that will keep the max value of score until all specialists have sent
   * a message
   *
   * @param response the product that have the most important nutrition contribution to the preexisting menu
   */
  def manageSpecificSpecialistResponse(response: Option[(Double, Product)]): Unit = {
    this.specificSpecialistResponseCount += 1

    // replace memorized product only if its none or if its score is inferior to a new one
    response match {
      case Some(r: (Double, Product)) =>
        if (this.bestProductSoFar.isEmpty || r._1 > this.bestProductSoFar.get._1) {
          this.bestProductSoFar = Some(r)
        }
      case None =>
    }

    // check if every specialists have send something
    if (this.specificSpecialistResponseCount == this.getSpecialistRefList.length) {
      // reset every memorized information
      this.specificSpecialistResponseCount = 0
      // objective : (bonus) manually dispose of child actors
      for (s <- this.getSpecialistRefList) {
        context.stop(s)
      }
      this.specialistRefList = Nil

      coq.get ! OtherIngredientResponse(this.bestProductSoFar.map(_._2))
      // of course the best product have to be reset after being sent
      this.bestProductSoFar = None
    }
  }
}

class Specialist(val name: String, val code: Int) extends Actor {

  /**
   * Check which product provide the best contribution to the existing menu regarding the specialist specification
   *
   * @param menu        the existing menu
   * @param allProducts the product from which the specialist must choose the next ingredient
   * @return the product that have best nutritional score contribution
   */
  def provideBestProduct(menu: Menu, allProducts: Seq[Product]): Option[(Double, Product)] = {
    // filter out the products that are inside the menu
    val choices = allProducts.filterNot(p => menu.products.map(_.id).toSet.contains(p.id))

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
