import scala.util.Random

object Main {
  def ask_for_int(message: String): Int = {
    var input: Option[Int] = None
    while (input.isEmpty) {
      input = scala.io.StdIn.readLine(message).toIntOption
    }
    input.get
  }

  def main(args: Array[String]) {
    val products: List[Product] = GastroExtractor.extract_products()
    val ajrs = GastroExtractor.extract_ajr()
    val menu_composer = new MenuComposer(products, ajrs)

    var n = 0
    while (true) {
      n = ask_for_int("How many items do you want in your menu ?")
      menu_composer.compose(n)
      scala.io.StdIn.readLine("Not satisfied? Hit any key to try another random menu composition out!")
    }
  }
}

class MenuComposer(products: List[Product], ajrs: List[AJRLimit]) {
  private def random_item(n: Int): List[Product] = {
    (for {
      _ <- 1 to n
    } yield products((new Random).nextInt.abs % products.length)).toList
  }

  def compose(n: Int): Unit = {
    val begin_with = random_item(n / 2)
    val end_products = better_compose(begin_with, products, n)

    println("----- Your menu is composed with the following " + n + " products:")
    end_products.foreach(println)

  }

  def better_compose(start_products: List[Product], list_products: List[Product], n: Int): List[Product] = {
    //    if (start_products.length >= 3 || list_products.isEmpty) {
    if (start_products.length >= n) {
      return start_products
    }

    val filtered = for {
      p <- list_products
      if !start_products.contains(p)
      if p.validate(ajrs)
    } yield p

    for {
      p <- filtered
      elem <- better_compose(p :: start_products, filtered, n)
    } yield elem
  }
}

class ProductProperty(id: Int, name: String, value: AJRValue) {
  def get_value: AJRValue = value

  def get_id: Int = id
}

class Product(id: Int, name: String, properties: List[ProductProperty]) {
  override def toString: String = name + " (" + id.toString + ")"

  def validate(ajrs: List[AJRLimit]): Boolean = {
    val is_inferior_to_max = for {
      ajr <- ajrs
      value <- properties
      if ajr.get_id == value.get_id
      result = value.get_value <= ajr.get_maximum

    } yield result

    val tab = if (ajrs.length == 3) {
      val is_superior_to_min = for {
        ajr <- ajrs
        value <- properties
        if ajr.get_id == value.get_id
        result = value.get_value >= ajr.get_minimum
      } yield result
      (is_superior_to_min zip is_inferior_to_max) map { case (a, b) => a && b }
    } else {
      is_inferior_to_max
    }

    tab.forall(i => i)
  }
}


case class AJRUnit(name: String) {
}


class AJRValue(value: Double, unit: AJRUnit) {
  override def toString: String = value + " (" + unit.name + ")"

  def normalized_value: Double = {
    val factor: Double = unit match {
      case AJRUnit("g") => 1
      case AJRUnit("mg") => 0.0001
      case AJRUnit("µg") => 0.0000001 // U+00B5 : MICRO SIGN (used when I type a "mu" with my keyboard)
      case AJRUnit("μg") => 0.0000001 // U+03BC : GREEK SMALL LETTER MU (used by some data c/c from internet)
      case _ =>
        println("The error comes from the " + unit + " unit")
        throw new Exception("unit not managed")
    }
    value * factor
  }

  def <=(other: AJRValue): Boolean = {
    this.normalized_value <= other.normalized_value
  }

  def <(other: AJRValue): Boolean = {
    this.normalized_value < other.normalized_value
  }

  def >=(other: AJRValue): Boolean = {
    this.normalized_value >= other.normalized_value
  }

  def >(other: AJRValue): Boolean = {
    this.normalized_value > other.normalized_value
  }

  def ==(other: AJRValue): Boolean = {
    this.normalized_value == other.normalized_value
  }

}

class AJRLimit(name: String, recommended_value: AJRValue, minimum: AJRValue, maximum: AJRValue, id: Int) {
  override def toString: String = name + "(" + id + ")" + " [" + minimum + " < " + recommended_value + " < " + maximum + "]"

  def get_id: Int = id

  def get_name: String = name

  def get_recommended: AJRValue = recommended_value

  def get_minimum: AJRValue = minimum

  def get_maximum: AJRValue = maximum
}


object GastroExtractor {
  def get_unit_from_product_col_id(id: Int): AJRUnit = {
    id match {
      case 14 => AJRUnit("µg")
      case 21 => AJRUnit("mg")
      case 22 => AJRUnit("mg")
      case 23 => AJRUnit("mg")
      case 24 => AJRUnit("mg")
      case 25 => AJRUnit("µg")
      case 30 => AJRUnit("µg")
      case 32 => AJRUnit("mg")
      case 33 => AJRUnit("µg")
      case 34 => AJRUnit("mg")
      case 36 => AJRUnit("µg")
      case 37 => AJRUnit("mg")
      case 38 => AJRUnit("mg")
      case 39 => AJRUnit("mg")
      case 40 => AJRUnit("mg")
      case 41 => AJRUnit("mg")
      case 42 => AJRUnit("mg")
      case 43 => AJRUnit("µg")
      case 44 => AJRUnit("mg")
      case 45 => AJRUnit("mg")
      case _ => throw new Exception("I honestly can't say :'(")
    }
  }

  def extract_products(): List[Product] = {
    val product_file = scala.io.Source.fromFile("data/products.csv")

    (for {
      product_line <- product_file.getLines.drop(1)
      product_cols = product_line.split(";")
      tab = for {
        ajr <- extract_ajr()
      } yield new ProductProperty(ajr.get_id, ajr.get_name, new AJRValue(product_cols(ajr.get_id).trim.toDouble, this.get_unit_from_product_col_id(ajr.get_id)))


    } yield new Product(
      product_cols(0).toInt,
      product_cols(1),
      tab)).toList
  }

  def extract_ajr(): List[AJRLimit] = {
    val ajr_file = scala.io.Source.fromFile("data/ajr.csv")
    (for {
      ajr_line <- ajr_file.getLines.drop(1)
      if !ajr_line.contains("-") // remove lines that contains missing values
      ajr_cols = ajr_line.split(";")
      unit = AJRUnit(ajr_cols(1).trim)
    } yield new AJRLimit(
      ajr_cols(0).trim,
      new AJRValue(ajr_cols(3).trim.toDouble, unit),
      new AJRValue(ajr_cols(4).trim.toDouble, unit),
      new AJRValue(ajr_cols(6).trim.toDouble, unit),
      ajr_cols(8).trim.toInt)
      ).toList
  }
}

