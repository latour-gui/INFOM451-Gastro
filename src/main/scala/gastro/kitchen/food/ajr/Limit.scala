package gastro.kitchen.food.ajr


/**
 * Representation of the data contained in the `ajr.csv` file
 *
 * @param name             the name of the element (vitamin, iron, ...)
 * @param recommendedValue the recommended value for this element
 * @param minimum          the minimum value for this element
 * @param maximum          the maximum value for this element
 * @param id               the column of the corresponding property in the `products.csv` file
 */
class Limit(val name: String, val recommendedValue: Value, val minimum: Value, val maximum: Value, val id: Int) {
  override def toString: String = name + "(" + id + ")" + " [" + minimum + " < " + recommendedValue + " < " + maximum + "]"
}