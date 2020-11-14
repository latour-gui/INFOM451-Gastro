package gastro.kitchen.employees

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import gastro.kitchen._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

/**
 * The MasterChef class is responsible for generating combinations of products and sending the valid ones as a LazyList
 *
 * @param productFilePath The file containing products at our disposition to compose menus
 * @param ajrFilePath     The file containing the gastro.kitchen.food.ajr.Limit that we use to validate the menus
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
      this.assistant = context.system.actorOf(Props[Assistant], "Assistant")
      this.client = sender()
      val specialist = context.system.actorOf(Props[SpecialistManager], "Specialist")

      // objective : (bonus) use the ask pattern
      implicit val timeout: Timeout = Timeout(1.seconds)
      val coreItemFuture = assistant ? CoreProductMessage

      // objective : manipulate future via callback
      coreItemFuture.onComplete {
        case Success(CoreProductResponse(coreItem)) =>
          this.menu = new Menu(Seq(coreItem))
          askForNextProductToSpecialist(specialist, this.menu)

        case Failure(exception) =>
          print(exception.getMessage)
          this.client ! NewMenuResponse(None)
      }


    //    case CoreProductResponse(p) => print("not needed because this case is managed through the ask pattern")


    case OtherIngredientResponse(p) =>
      this.menu = new Menu(this.menu.products :+ p)
      askForNextProductToSpecialist(sender(), this.menu)


    case QuantityResponse(what) =>
      this.client ! NewMenuResponse(Some(this.menu))

    case _ => println("nique")
  }

  def askForNextProductToSpecialist(specialist: ActorRef, menu: Menu): Unit = {
    if (menu.products.length < this.maxProductInMenu) {
      specialist ! OtherIngredientMessage(menu)
    } else {
      validate()
    }
  }

  def validate(): Unit = {
    this.assistant ! QuantityMessage(this.menu)
  }

  //
  //  /**
  //   * Generate all the possible combinations of n products from all of those at our disposition
  //   *
  //   * It is really important that the return type is LazyList instead of List.
  //   * Because if the List was evaluated at this function call, it's most likely to block the threat for a long time
  //   *
  //   * @param n the number of products that should be in the combination
  //   * @return lazy evaluated list of the combinations of n product from the given list
  //   */
  //  def allProductCombinations(n: Int): LazyList[Menu] = {
  //    // objective : abuse for comprehension
  //    LazyList.from(for {
  //      m <- Random.shuffle(this.products).combinations(n)
  //    } yield new Menu(m))
  //  }
  //
  //  /**
  //   * Compose a lazy evaluated list of menu that do respect the daily recommended intake
  //   *
  //   * @param n the number of product in the menu
  //   * @return lazy evaluated list of menu
  //   */
  //  def compose(n: Int): LazyList[Menu] = {
  //    // objective : overdose for comprehension
  //    for {
  //      menu <- allProductCombinations(n)
  //      if menu.validate(this.ajrs)
  //    } yield menu
  //  }
}
