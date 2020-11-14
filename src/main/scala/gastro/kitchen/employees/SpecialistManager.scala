package gastro.kitchen.employees

import akka.actor.Actor
import gastro.Main.promptMessage
import gastro.kitchen._
import gastro.kitchen.food._

import scala.math.{pow, sqrt}
import scala.util.{Failure, Success}

class SpecialistManager() extends Actor {
  var specialistList: Seq[Specialist] = Nil

  def getSpecialistList: Seq[Specialist] = {
    if (this.specialistList.isEmpty) {
      this.specialistList = rememberAJR() match {
        case Failure(exception) =>
          promptMessage(exception.getMessage)
          Nil
        case Success(ajrs) =>
          // generate the different specialists : 1 by ajr type
          val specialists = ajrs
            .map(ajr => Specialist(ajr.name, ajr.id))

          // allow each specialist to gather information about his field
          // objective : another kind of for comprehension usage
          for (products <- lookForProducts(ajrs);
               product <- products;
               property <- product.getProperties;
               specialist <- specialists
               if specialist.code == property.id) {
            specialist.study(property)
          }
          for (products <- lookForProducts(ajrs); specialist <- specialists) {
            specialist.classProduct(products)
          }

          specialists
      }
    }
    this.specialistList
  }

  override def receive: Receive = {
    case OtherIngredientMessage(m) =>
      sender ! OtherIngredientResponse(consultSpecialistsForNextIngredient(m))

    case _ => println("Specialist received an incomprehensible message")
  }


  def consultSpecialistsForNextIngredient(menu: Menu): Option[Product] =
    getSpecialistList.map(_.provideBestProduct(menu)).sortBy(_._1).reverse.headOption.map(_._2)

}

case class Specialist(name: String, code: Int) {
  var knownValues: Seq[ajr.Value] = Nil
  var lowP: Seq[Product] = Nil
  var avgP: Seq[Product] = Nil
  var highP: Seq[Product] = Nil

  def mean: ajr.Value = new ajr.Value(knownValues.map(_.normalizedValue).sum / knownValues.length, ajr.Unit("g"))

  var sd: ajr.Value = {
    val avg = mean
    new ajr.Value(sqrt(knownValues.map(v => pow(v.normalizedValue - avg.normalizedValue, 2)).sum / knownValues.length), ajr.Unit("g"))
  }

  def study(p: ProductProperty): Unit = {
    this.knownValues = this.knownValues :+ p.value
  }

  def classProduct(products: Seq[Product]): Unit = {
    val avg = mean
    val standardDeviation = sd
    for (p <- products;
         property <- p.getProperties
         if property.id == this.code) {
      if (property.value <= (avg - standardDeviation)) {
        lowP = lowP :+ p
      } else if (property.value < (avg + standardDeviation)) {
        highP = highP :+ p
      } else {
        avgP = avgP :+ p
      }
    }
  }

  def provideBestProduct(menu: Menu): (Double, Product) = {
    var choices = highP.filter(p => !menu.products.contains(p))
    if (choices.isEmpty) {
      choices = avgP.filter(p => !menu.products.contains(p))
    }
    if (choices.isEmpty) {
      choices = lowP.filter(p => !menu.products.contains(p))
    }
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
