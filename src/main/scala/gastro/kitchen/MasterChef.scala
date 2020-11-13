package gastro.kitchen

import gastro.kitchen.food.{Product, ProductProperty, ajr}
import gastro.utils.CSV

import scala.util.{Failure, Random, Success}

/**
 * The MasterChef class is responsible for generating combinations of products and sending the valid ones as a LazyList
 *
 * @param productFilePath The file containing products at our disposition to compose menus
 * @param ajrFilePath     The file containing the gastro.kitchen.food.ajr.Limit that we use to validate the menus
 */
class MasterChef(productFilePath: String, ajrFilePath: String) {

  val products: Seq[Product] = lookForProduct(productFilePath)
  val ajrs: Seq[ajr.Limit] = rememberAJR(ajrFilePath)

  def lookForProduct(filePath: String): Seq[Product] =
    for {
      headerValueMap: Map[String, String] <- CSV.extract(filePath) match {
        case Success(value) => value
        case Failure(exception) => throw exception
      }
      ajrsAsProductProperties = for {
        a <- ajrs
        value <- headerValueMap.get(ajr.getProductColumnNameFromAjrId(a.id)).flatMap(_.trim.toDoubleOption)
      } yield new ProductProperty(a.id, a.name, new ajr.Value(value, ajr.getUnitFromProductColId(a.id)))
      id <- headerValueMap.get("Food code").flatMap(_.trim.toIntOption)
      name <- headerValueMap.get("Main food description")
    } yield new Product(
      id,
      name,
      ajrsAsProductProperties)

  def rememberAJR(filePath: String): Seq[ajr.Limit] =
    for {
      headerValueMap: Map[String, String] <- CSV.extract(filePath) match {
        case Success(value) => value
        case Failure(exception) => throw exception
      }

      if !headerValueMap.values.exists(_ == "-") // remove lines that contains missing values

      // extract some values (manipulation of Option[X] as if they were X thanks to for comprehension)
      // objective : Option manipulation (inside for comprehension)
      unitString <- headerValueMap.get("unité").map(_.trim)
      unit = ajr.Unit(unitString)

      recommendedValue <- headerValueMap.get("Valeur AJR de la directive 2008/100/CE").flatMap(_.trim.toDoubleOption)
      minimumValue <- headerValueMap.get("Minimum").flatMap(_.trim.toDoubleOption)
      maximumValue <- headerValueMap.get("Maximum").flatMap(_.trim.toDoubleOption)
      name <- headerValueMap.get("Nom nutriment").map(_.trim)
      id <- headerValueMap.get("column in product").flatMap(_.trim.toIntOption)
    } yield new ajr.Limit(
      name,
      new ajr.Value(recommendedValue, unit),
      new ajr.Value(minimumValue, unit),
      new ajr.Value(maximumValue, unit),
      id)


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
    // objective : abuse for comprehension
    LazyList.from(for {
      m <- Random.shuffle(this.products).combinations(n)
    } yield new Menu(m))
  }

  /**
   * Compose a lazy evaluated list of menu that do respect the daily recommended intake
   *
   * @param n the number of product in the menu
   * @return lazy evaluated list of menu
   */
  def compose(n: Int): LazyList[Menu] = {
    // objective : overdose for comprehension
    for {
      menu <- allProductCombinations(n)
      if menu.validate(this.ajrs)
    } yield menu
  }
}
