import scala.util.Random


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
    val products: Seq[Product] = GastroExtractor.extractProduct()
    val ajrs = GastroExtractor.extractAjr()
    val menuComposer = new MenuComposer(products, ajrs)

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

//<editor-fold desc="MENU">

/**
 * The `Menu` class contains computing logic that allows one to know if the menu is valid in the point of view of the
 * recommended daily intake.
 *
 * @param products the products contained in the menu
 */
class Menu(products: Seq[Product]) {
  override def toString: String = "The menu is composed by the following elements :\n\t- " + products.map(_.toString).mkString("\n\t- ")

  /**
   * Sum the value of each product properties
   *
   * This allows easy manipulation of all the ProductProperty's AJRValue contained in the menu.
   *
   * @return
   */
  def totalProperties: Seq[ProductProperty] = {
    products
      .flatMap(_.getProperties)
      .groupBy(_.getId)
      .map {
        case (id: Int, properties: Seq[ProductProperty]) => new ProductProperty(
          id,
          "unnecessary",
          // sum the AJRValues of properties with fold
          properties.map(_.getValue).fold(new AJRValue(0, AJRUnit("g"))) { case (acc, elem) => acc + elem }
        )
      }

      .toList
  }

  /**
   * Logic behind the validation of a menu
   *
   * The menu must
   *  - contain only unique products
   *  - all the ProductProperty's values must be between min and max AJRValue
   *
   * @param ajrs The list of the Daily Recommended Intake (Apport Journalier Recommandé in french)
   * @return true if the menu is valid, false otherwise
   */
  def validate(ajrs: Seq[AJRLimit]): Boolean = uniqueProduct && validAjr(ajrs)

  /**
   * Check if the products in the menu are all different
   *
   * @return true if every product appears only once, false otherwise
   */
  def uniqueProduct: Boolean = products.length == products.map(_.getId).distinct.length

  /**
   * Check if the addition of all the products in the menu do respect the Daily Recommended Intake
   *
   * @param ajrs The list of the AJRLimit wich determine the daily recommended intake
   * @return true if its ok, false otherwise
   */
  def validAjr(ajrs: Seq[AJRLimit]): Boolean = {
    (for {
      ajr <- ajrs
      value <- totalProperties
      if ajr.getId == value.getId
      v = value.getValue
      result = v <= ajr.getMaximum && v >= ajr.getMinimum
    } yield result).forall(i => i)
  }
}

/**
 * The MenuCompose class is responsible for generating combinations of products and sending the valid ones as a LazyList
 *
 * @param products The products at our disposition to compose menus
 * @param ajrs     The AJRLimit that we use to validate the menus
 */
class MenuComposer(products: Seq[Product], ajrs: Seq[AJRLimit]) {

  /**
   * Generate all the possible combinations of n products from all of those at our disposition
   *
   * It is really important that the return type is LazyList instead of List.
   * Because if the List was evaluated at this function call, it's most likely to block the threat for a long time
   *
   * @param n the number of products that should be in the combination
   * @return lazy evaluated list of the combinations of n product from the given list
   */
  def allProductCombinations(n: Int): LazyList[Menu] = {
    LazyList.from(for {
      m <- Random.shuffle(products).combinations(n)
    } yield new Menu(m))
  }

  /**
   * Compose a lazy evaluated list of menu that do respect the daily recommended intake
   *
   * @param n the number of product in the menu
   * @return lazy evaluated list of menu
   */
  def compose(n: Int): LazyList[Menu] = {
    for {
      menu <- allProductCombinations(n)
      if menu.validate(ajrs)
    } yield menu
  }
}

// </editor-fold>
//<editor-fold desc="PRODUCT">

/**
 * Property of a product
 *
 * This is only a hollow class that helps the manipulation of data by giving them meaning
 * ty captain obvious, its a class
 *
 * @param id    the number of the column of the property in the `products.csv`
 * @param name  the name of the property (head of column in productrs.csv)
 * @param value the value of the property
 */
class ProductProperty(id: Int, name: String, value: AJRValue) {
  def getValue: AJRValue = value

  def getId: Int = id
}

/**
 * Product
 *
 * Ease manipulation of the product data
 *
 * @param id         the id of the product (col 0 of products.csv)
 * @param name       the name of the product (col 1 if products.csv)
 * @param properties interesting properties (cols) of the file products.csv
 */
class Product(id: Int, name: String, properties: Seq[ProductProperty]) {
  override def toString: String = name + " (" + id.toString + ")"

  def getId: Int = id

  def getProperties: Seq[ProductProperty] = properties
}

// </editor-fold>
//<editor-fold desc="AJR">

/**
 * Case class to allow pattern matching on different units
 *
 * @param name the name of the unit (g, mg, µg)
 */
case class AJRUnit(name: String)

/**
 * The combination of a value and a unit
 *
 * The big idea is to allow comparison and addition over values that have different unit in the csv files.
 *
 * @param value the value
 * @param unit  the unit
 */
class AJRValue(value: Double, unit: AJRUnit) {
  override def toString: String = value + " (" + unit.name + ")"

  /**
   * Return the normalized value (base unit is gram [g])
   *
   * @return the value converted in gram
   */
  def normalizedValue: Double = {
    val factor: Double = unit match {
      case AJRUnit("g") => 1
      case AJRUnit("mg") => 0.0001
      case AJRUnit("µg") => 0.0000001 // U+00B5 : MICRO SIGN (used when I type a "mu" with my keyboard)
      case AJRUnit("μg") => 0.0000001 // U+03BC : GREEK SMALL LETTER MU (used by some data c/p from internet)
      case _ =>
        println("The error comes from the " + unit + " unit")
        throw new Exception("unit not managed")
    }
    value * factor
  }

  def <=(other: AJRValue): Boolean = {
    this.normalizedValue <= other.normalizedValue
  }

  def <(other: AJRValue): Boolean = {
    this.normalizedValue < other.normalizedValue
  }

  def >=(other: AJRValue): Boolean = {
    this.normalizedValue >= other.normalizedValue
  }

  def >(other: AJRValue): Boolean = {
    this.normalizedValue > other.normalizedValue
  }

  def ==(other: AJRValue): Boolean = {
    this.normalizedValue == other.normalizedValue
  }

  def +(other: AJRValue): AJRValue = {
    new AJRValue(this.normalizedValue + other.normalizedValue, AJRUnit("g"))
  }
}

/**
 * Representation of the data contained in the `ajr.csv` file
 *
 * @param name             the name of the element (vitamine, iron, ...)
 * @param recommendedValue the recommended value for this element
 * @param minimum          the minimum value for this element
 * @param maximum          the maximum value for this element
 * @param id               the column of the corresponding property in the `products.csv` file
 */
class AJRLimit(name: String, recommendedValue: AJRValue, minimum: AJRValue, maximum: AJRValue, id: Int) {
  override def toString: String = name + "(" + id + ")" + " [" + minimum + " < " + recommendedValue + " < " + maximum + "]"

  def getId: Int = id

  def getName: String = name

  def getRecommended: AJRValue = recommendedValue

  def getMinimum: AJRValue = minimum

  def getMaximum: AJRValue = maximum
}

// </editor-fold>
//<editor-fold desc="EXTRACTORS">

/**
 * Object that is responsible for the extraction of data from the csv files
 */
object GastroExtractor {
  /**
   * Get the correspondence between the index of the column of the `products.csv` file and the unit
   *
   * @param id the column index
   * @return the AJRUnit of that column
   */
  def getUnitFromProductColId(id: Int): AJRUnit = {
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

  /**
   * Logic to extract the list of product from the `product.csv` file
   *
   * @return
   */
  def extractProduct(): Seq[Product] = {
    val productFile = scala.io.Source.fromFile("data/products.csv")

    (for {
      productLine <- productFile.getLines.drop(1)
      productCols = productLine.split(";")
      tab = for {
        ajr <- extractAjr()
      } yield new ProductProperty(ajr.getId, ajr.getName, new AJRValue(productCols(ajr.getId).trim.toDouble, this.getUnitFromProductColId(ajr.getId)))


    } yield new Product(
      productCols(0).toInt,
      productCols(1),
      tab)).toList
  }

  /**
   * Logic to extract the AJRLimit from the `ajr.csv` file
   *
   * @return
   */
  def extractAjr(): Seq[AJRLimit] = {
    val ajrFile = scala.io.Source.fromFile("data/ajr.csv")
    (for {
      ajrLine <- ajrFile.getLines.drop(1)
      if !ajrLine.contains("-") // remove lines that contains missing values
      ajrCols = ajrLine.split(";")
      unit = AJRUnit(ajrCols(1).trim)
    } yield new AJRLimit(
      ajrCols(0).trim,
      new AJRValue(ajrCols(3).trim.toDouble, unit),
      new AJRValue(ajrCols(4).trim.toDouble, unit),
      new AJRValue(ajrCols(6).trim.toDouble, unit),
      ajrCols(8).trim.toInt)
      ).toList
  }
}

// </editor-fold>