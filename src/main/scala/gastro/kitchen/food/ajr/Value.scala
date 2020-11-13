package gastro.kitchen.food.ajr

import gastro.utils.errors.CustomException

// objective : define a class
/**
 * The combination of a value and a unit
 *
 * The big idea is to allow comparison and addition over values that have different unit in the csv files.
 *
 * @param value the value
 * @param unit  the unit
 */
class Value(value: Double, unit: Unit) {
  override def toString: String = value + " (" + unit.name + ")"

  /**
   * Return the normalized value (base unit is gram [g])
   *
   * @return the value converted in gram
   */
  def normalizedValue: Double = {
    // objective : use pattern matching on case class
    val factor: Double = unit match {
      case Unit("g") => 1
      case Unit("mg") => 0.0001
      case Unit("µg") => 0.0000001 // U+00B5 : MICRO SIGN (used when I type a "mu" with my keyboard)
      case Unit("μg") => 0.0000001 // U+03BC : GREEK SMALL LETTER MU (used by some data c/p from internet)
      case _ =>
        println("The error comes from the " + unit + " unit")
        throw new CustomException("unit not managed")
    }
    value * factor
  }

  def <=(other: Value): Boolean = {
    this.normalizedValue <= other.normalizedValue
  }

  def <(other: Value): Boolean = {
    this.normalizedValue < other.normalizedValue
  }

  def >=(other: Value): Boolean = {
    this.normalizedValue >= other.normalizedValue
  }

  def >(other: Value): Boolean = {
    this.normalizedValue > other.normalizedValue
  }

  def ==(other: Value): Boolean = {
    this.normalizedValue == other.normalizedValue
  }

  def +(other: Value): Value = {
    new Value(this.normalizedValue + other.normalizedValue, Unit("g"))
  }
}