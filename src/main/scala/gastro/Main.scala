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
   * This will infinitely ask the user his/her name and launch the menu ordering process
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

  /**
   * Binding to allow a user to input an string
   *
   * @param message The message that will be displayed to the user while waiting for a string
   * @return The string entered by the user
   */
  def askForString(message: String): String = {
    scala.io.StdIn.readLine(message + "\n\t> ")
  }

  /**
   * Interact with the "kitchen" in an actor model fashion.
   * A menu for the `orderName` is composed by the kitchen
   *
   * @param orderName The name of the order
   */
  def askForMenu(orderName: String): Unit = {
    val n: Int = askForInt("Hello " + orderName + ", please have a seat.\nHow many items do you want in your menu ?")

    // objective : actor model usage
    // creation of the actor system and initialization of the Coq
    val system = ActorSystem("kitchen")
    val coq = system.actorOf(Props[Coq], "Coq")

    // objective : (bonus) ask pattern usage
    implicit val timeout: Timeout = Timeout(10.seconds)
    val menu = coq ? NewMenuMessage(n)

    promptMessage("A menu has been ordered for the name '" + orderName + "'.")

    // objective : Future management
    menu.onComplete {
      case Success(NewMenuResponse(Some(menu))) =>
        promptMessage("The '" + orderName + "' menu is prepared\n\n" + menu.toString)
      case Success(NewMenuResponse(None)) =>
        promptMessage("The kitchen was not able to produce a decent menu for '" + orderName + "'")
    }
  }

  /**
   * Wrapper to pass messages "from the kitchen"
   *
   * The message will appear in a box-style in stdout
   *
   * @param message the message that needs to be displayed
   */
  def promptMessage(message: String): Unit = {
    val separator = "+------------- - - - "

    print("\n" + separator + "(message from kitchen)" + "\n" +
      message.split("\n").map(m => "| " + m).mkString("\n") +
      "\n" + separator + "\n")
  }
}
