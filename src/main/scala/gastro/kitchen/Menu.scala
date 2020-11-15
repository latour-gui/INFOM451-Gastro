package gastro.kitchen

import gastro.kitchen.food.{Product, ProductProperty, QuantifiedProduct, ajr}

/**
 * Immutable data structure that allows grouping of `Product`s
 *
 * @param products the products contained in the menu
 */
class Menu(val products: Seq[Product]) {
  override def toString: String = "The menu is composed by the following elements :\n\t- " + products.groupBy(_.id).map({
    case (id: Int, products: Seq[Product]) => products.length + " " + products.head.name + " (" + id + ")"
  }).mkString("\n\t- ")

  /**
   * Sum the value of each product properties
   *
   * This allows easy manipulation of all the ProductProperty's ajr.Value contained in the menu.
   *
   * @return List of product property that are the sum of each product's
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
          // sum the ajr.Values of properties with fold
          // another way of doing so would use the `sumBy` method
          // objective : use anonymous function
          properties.map(_.value).fold(new ajr.Value(0, ajr.Unit("g")))(_ + _)
        )
      }
      // objective : discover immutable data structure (scala.collection.immutable.List[A])
      .toList
  }
}

/**
 * Immutable data structure that allows grouping of `QuantifiedProduct`s
 *
 * @param products The products contained in the quantified menu
 */
case class QuantifiedMenu(products: Seq[QuantifiedProduct]) {
  override def toString: String = "The menu is composed by the following elements :\n\t- " +
    products
      .map(qp => qp.portion + " of " + qp.product.name + " (" + qp.weight + "g)")
      .mkString("\n\t- ")
}