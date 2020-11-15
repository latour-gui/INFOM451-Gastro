package gastro

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import gastro.kitchen._
import gastro.kitchen.employees.Coq

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Success

object Main {

  /**
   * Entrypoint of the program.
   *
   * This will load two files via the corresponding extractors,
   * then it will ask how many items the user wants in its menu,
   * then it will propose all the combinations of the products that respects the Recommended daily intake.
   * Between each combination, the program expect the user to manifest its intention to keep displaying menus.
   *
   * @param args The parameters of the program
   */
  def main(args: Array[String]) {
    var name: String = ""
    while (true) {
      name = askForString("Welcome at our Gastro kitchen.\nWhich name should I use for your order ?")
      askForMenu(name)
    }
  }

  /**
   * Binding to allow a user to input an integer
   *
   * This will loop until the user is able to enter an integer
   *
   * @param message The message that will be displayed to the user while waiting for an integer
   * @return The integer entered by the user
   */
  def askForInt(message: String): Int = {
    var input: Option[Int] = None
    while (input.isEmpty) {
      input = scala.io.StdIn.readLine(message + "\n\t> ").toIntOption
    }
    input.get
  }

  def askForString(message: String): String = {
    scala.io.StdIn.readLine(message + "\n\t> ")
  }

  def askForMenu(orderName: String): Unit = {
    val n: Int = askForInt("Hello " + orderName + ", please have a seat.\nHow many items do you want in your menu ?")

    val system = ActorSystem("kitchen")
    val coq = system.actorOf(Props[Coq], "Coq")
    implicit val timeout: Timeout = Timeout(10.seconds)
    val menu = coq ? NewMenuMessage(n)
    promptMessage("A menu has been ordered for the name '" + orderName + "'.")

    menu.onComplete {
      case Success(NewMenuResponse(Some(menu))) =>
        promptMessage("The '" + orderName + "' menu is prepared\n\n" + menu.toString)
      case Success(NewMenuResponse(None)) =>
        promptMessage("The kitchen was not able to produce a decent menu for '" + orderName + "'")
    }
  }

  def promptMessage(message: String): Unit = {
    val separator = "+------------- - - - "

    print("\n" + separator + "(message from kitchen)" + "\n" +
      message.split("\n").map(m => "| " + m).mkString("\n") +
      "\n" + separator + "\n")
  }
}
