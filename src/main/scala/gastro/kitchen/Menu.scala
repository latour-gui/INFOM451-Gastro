package gastro.kitchen

import gastro.kitchen.food.{Product, ProductProperty, ajr}

/**
 * The `gastro.menu.Menu` class contains computing logic that allows one to know if the menu is valid in the point of view of the
 * recommended daily intake.
 *
 * @param products the products contained in the menu
 */
class Menu(val products: Seq[Product]) {
  override def toString: String = "The menu is composed by the following elements :\n\t- " + products.map(_.toString).mkString("\n\t- ")

  /**
   * Sum the value of each product properties
   *
   * This allows easy manipulation of all the gastro.menu.ProductProperty's gastro.kitchen.food.ajrValue contained in the menu.
   *
   * @return
   */
  def totalProperties: Seq[ProductProperty] = {
    // objective : use high level methods
    products
      .flatMap(_.getProperties)
      .groupBy(_.id)
      .map {
        case (id: Int, properties: Seq[ProductProperty]) => new ProductProperty(
          id,
          properties.head.name, // those are all the same properties (because they have been grouped by id)
          // objective : use anonymous function
          // sum the ajr.Values of properties with fold
          properties.map(_.value).fold(new ajr.Value(0, ajr.Unit("g")))(_ + _)
        )
      }
      // objective : discover immutable data structure
      .toList
  }

  /**
   * Logic behind the validation of a menu
   *
   * The menu must
   *  - contain only unique products
   *  - all the gastro.menu.ProductProperty's values must be between min and max gastro.kitchen.food.ajrValue
   *
   * @param ajrs The list of the Daily Recommended Intake (Apport Journalier Recommandé in french)
   * @return true if the menu is valid, false otherwise
   */
  def validate(ajrs: Seq[ajr.Limit]): Boolean = uniqueProduct && validAjr(ajrs)

  /**
   * Check if the products in the menu are all different
   *
   * @return true if every product appears only once, false otherwise
   */
  def uniqueProduct: Boolean = products.length == products.map(_.id).distinct.length

  /**
   * Check if the addition of all the products in the menu do respect the Daily Recommended Intake
   *
   * @param ajrs The list of the gastro.kitchen.food.ajrLimit which determine the daily recommended intake
   * @return true if its ok, false otherwise
   */
  def validAjr(ajrs: Seq[ajr.Limit]): Boolean = {
    // objective : use for comprehension
    (for {
      ajr <- ajrs
      property <- totalProperties
      if ajr.id == property.id
      v = property.value
      result = v < ajr.maximum && v > ajr.minimum
    } yield result).forall(i => i)
  }
}
