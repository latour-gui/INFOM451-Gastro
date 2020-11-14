package gastro.kitchen.employees

import akka.actor.Actor
import gastro.kitchen._
import gastro.kitchen.food.Product

class SpecialistManager() extends Actor {
  override def receive: Receive = {
    case OtherIngredientMessage(m) =>
      sender ! OtherIngredientResponse(consultSpecialistsForNextIngredient(m))

    case _ => println("Specialist received an incomprehensible message")
  }

  def consultSpecialistsForNextIngredient(menu: Menu): Product = new Product(1, "skeleton", Nil)
}
