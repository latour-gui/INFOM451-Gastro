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
 * The MasterChef class is responsible for generating combinations of products and sending the valid ones as a LazyList
 *
 */
//products: Seq[Product], ajrs: Seq[ajr.Limit]
class Coq() extends Actor {
  var menu: Menu = new Menu(Nil)
  var maxProductInMenu: Integer = 0
  var assistant: ActorRef = _
  var client: ActorRef = _

  def receive: Receive = {
    case NewMenuMessage(n) =>
      this.maxProductInMenu = n
      this.assistant = context.actorOf(Props[Assistant], "Assistant")
      this.client = sender()
      val specialist = context.actorOf(Props[SpecialistManager], "Specialist")

      // objective : (bonus) use the ask pattern
      implicit val timeout: Timeout = Timeout(1.seconds)
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


    //    case CoreProductResponse(p) => print("not needed because this case is managed through the ask pattern")


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

  def askForNextProductToSpecialist(specialist: ActorRef, menu: Menu): Unit = {
    if (menu.products.length < this.maxProductInMenu) {
      specialist ! OtherIngredientMessage(menu)
    } else {
      validate()
    }
  }

  def sendEmptyMenu(): Unit = this.client ! NewMenuResponse(None)

  def validate(): Unit = {
    this.assistant ! QuantityMessage(this.menu)
  }
}
