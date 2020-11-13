package gastro.kitchen.food


/**
 * gastro.menu.Product
 *
 * Ease manipulation of the product data
 *
 * @param id         the id of the product (col 0 of products.csv)
 * @param name       the name of the product (col 1 if products.csv)
 * @param properties interesting properties (cols) of the file products.csv
 */
class Product(val id: Int, val name: String, properties: Seq[ProductProperty]) {
  override def toString: String = name + " (" + id.toString + ")"

  def getProperties: Seq[ProductProperty] = properties
}

/**
 * Property of a product
 *
 * This is only a hollow class that helps the manipulation of data by giving them meaning
 * ty captain obvious, its a class
 *
 * @param id    the number of the column of the property in the `products.csv`
 * @param name  the name of the property (head of column in products.csv)
 * @param value the value of the property
 */
class ProductProperty(val id: Int, val name: String, val value: ajr.Value)
