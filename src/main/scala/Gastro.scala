import scala.util.Random

object Main {
  def main(args: Array[String]) {
    val products_file: String = "data/products.csv"
    val products: List[Product] = GastroExtractor.extract_products(products_file)
    println(products)
    val menu_composer = new MenuComposer(products)
    while (true) {
      println(menu_composer.compose)
      scala.io.StdIn.readLine("Not satisfied? Hit any key to try another random menu composition out!")
    }
  }
}

class MenuComposer(products: List[Product]) {
  private def n_random_products(): List[Product] = {
    for {_ <- List.range(0, 3)}
      yield products((new Random).nextInt.abs % products.length)
  }

  def compose: String = {
    val random_products = n_random_products()
    println("----- I will try assembling the following three products:")
    println("--- 1 --- " + random_products.head)
    println("--- 2 --- " + random_products(1))
    println("--- 3 --- " + random_products(2))

    if (random_products.map(_.quality_indicator).foldLeft(0.0)(_ + _) <= 1)
      "The three selected products are delicious together and form a healthy meal!"
    else
      "Beurk! never eat those together, it's either too fat, too sugarry or both..."
  }
}

class Product(id: Int, name: String, energy: Int, protein: Float) {
  override def toString: String = name + " (" + id.toString + ")"

  def quality_indicator: Double = if (energy < 30) 0.3 else {
    if (protein > 20) 0.2 else 0.6
  }
}

object GastroExtractor {
  def extract_products(path: String): List[Product] = {
    val file = scala.io.Source.fromFile(path)
    (for {
      line <- file.getLines.drop(1)
      cols = line.split(";")
    } yield new Product(cols(0).toInt, cols(1), cols(4).toInt, cols(5).toFloat)).toList
  }
}
