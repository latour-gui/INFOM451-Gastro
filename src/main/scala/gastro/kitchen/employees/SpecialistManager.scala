package gastro.kitchen.employees

import akka.actor.Actor
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food._

import scala.util.{Failure, Success}

class SpecialistManager() extends Actor {
  var specialistList: Seq[Specialist] = Nil
  var productList: Seq[Product] = Nil
  var ajrsList: Seq[ajr.Limit] = Nil

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
  def getSpecialistList: Seq[Specialist] = {
    if (this.specialistList.isEmpty) {
      // objective : manipulate method with a Try[T] signature
      this.specialistList = rememberAJR().map(_.map { a: ajr.Limit => Specialist(a.name, a.id) }) match {
        case Success(sl: Seq[Specialist]) => sl
        case Failure(exception) => promptMessage(exception.getMessage); Nil
      }
    }
    this.specialistList
  }

  override def receive: Receive = {
    case OtherIngredientMessage(m) =>
      sender ! OtherIngredientResponse(consultSpecialistsForNextIngredient(m))

    case _ => promptMessage("Specialist received an incomprehensible message")
  }


  def consultSpecialistsForNextIngredient(menu: Menu): Option[Product] =
    getSpecialistList.map(_.provideBestProduct(menu, this.productList)).sortBy(_._1).reverse.headOption.map(_._2)

}

case class Specialist(name: String, code: Int) {

  def provideBestProduct(menu: Menu, products: Seq[Product]): (Double, Product) = {
    val choices = products.filter(p => !menu.products.contains(p))

    if (choices.isEmpty) {
      return (0.0, null)
    }

    menu.totalProperties.find(_.id == this.code) match {
      case Some(productProperty) =>
        val interestingValue = productProperty.value.normalizedValue
        (for {
          p <- choices
          property <- p.getProperties
          if property.id == this.code
          v = property.value.normalizedValue
        } yield (v / (interestingValue + v), p)).maxBy(_._1)
      case None => (0.0, null)
    }

  }
}
