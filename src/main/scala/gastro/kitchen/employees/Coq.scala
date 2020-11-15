package gastro.kitchen.employees

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import gastro.Main.promptMessage
import gastro.kitchen._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

/**
 * The Coq class is responsible for taking order from the clients and dispatch the work into the kitchen.
 *
 */
class Coq() extends Actor {
  // begin by a menu with zero product
  var menu: Menu = new Menu(Nil)
  // will be set by client
  var maxProductInMenu: Integer = 0
  var assistant: ActorRef = _
  var client: ActorRef = _

  /**
   * Mandatory method for Actors.
   *
   * All the different messages that can be sent to the Coq are managed here.
   *
   */
  def receive: Receive = {
    case NewMenuMessage(n) =>
      this.maxProductInMenu = n
      this.assistant = context.actorOf(Props(new Assistant()), "Assistant")
      this.client = sender()
      val specialist = context.actorOf(Props[SpecialistManager], "SpecialistManager")

      // objective : (bonus) use the ask pattern
      implicit val timeout: Timeout = Timeout(2.seconds)
      val coreItemFuture = assistant ? CoreProductMessage

      // objective : manipulate future via callback
      coreItemFuture.onComplete {
        case Success(CoreProductResponse(coreItem)) =>
          coreItem match {
            case Some(item) =>
              this.menu = new Menu(Seq(item))
              askForNextProductToSpecialist(specialist, this.menu)
            case None =>
              sendEmptyMenu()
          }
        case Failure(exception) =>
          promptMessage(exception.getMessage)
          sendEmptyMenu()
      }


    //    case CoreProductResponse(p) => print("not needed because this case is managed through the ask pattern above")


    case OtherIngredientResponse(product) =>
      product match {
        case Some(p) => this.menu = new Menu(this.menu.products :+ p)
          askForNextProductToSpecialist(sender(), this.menu)
        case None =>
          promptMessage("The specialists do not know which ingredient to add...")
          sendEmptyMenu()
      }


    case QuantityResponse(finalMenu) =>
      this.client ! NewMenuResponse(Some(finalMenu))


    case _ => promptMessage("A message was not interpreted correctly by the Coq")
  }

  /**
   * Wrapper to call specialist manager for new product to add in menu
   *
   * @param specialistManager the actorRef to the specialist manager
   * @param menu              the menu that is composed right now
   */
  def askForNextProductToSpecialist(specialistManager: ActorRef, menu: Menu): Unit = {
    // check if the menu is not already of max size
    if (menu.products.length < this.maxProductInMenu) {
      specialistManager ! OtherIngredientMessage(menu)
    } else {
      validate(menu)
    }
  }

  /**
   * Send an empty menu to the client
   */
  def sendEmptyMenu(): Unit = this.client ! NewMenuResponse(None)

  /**
   * Ask the assistant for quantity in the finished menu
   *
   * @param m the menu for which we want the quantities
   */
  def validate(m: Menu): Unit = {
    this.assistant ! QuantityMessage(m)
  }
}
