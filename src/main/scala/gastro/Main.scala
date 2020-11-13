package gastro

import gastro.kitchen.MasterChef

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
    val productPath = "data/products.csv" // todo : if is set args['inputpath']
    val ajrPath = "data/ajr.csv" // todo : if isset args[ajr]
    val menuComposer = new MasterChef(productPath, ajrPath)

    val n: Int = askForInt("How many items do you want in your menu ?")
    for (meal <- menuComposer.compose(n)) {
      println(meal)
      scala.io.StdIn.readLine("Not satisfied? Hit any key to try another random menu composition out!")
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
      input = scala.io.StdIn.readLine(message).toIntOption
    }
    input.get
  }

}
