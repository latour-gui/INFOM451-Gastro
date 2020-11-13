package gastro.kitchen.food

package object ajr {
  /**
   * Get the correspondence between the index of the column of the `products.csv` file and the unit
   *
   * @param id the column index
   * @return the gastro.kitchen.food.ajr.Unit of that column
   */
  def getUnitFromProductColId(id: Int): ajr.Unit = {
    id match {
      case 14 => ajr.Unit("µg")
      case 21 => ajr.Unit("mg")
      case 22 => ajr.Unit("mg")
      case 23 => ajr.Unit("mg")
      case 24 => ajr.Unit("mg")
      case 25 => ajr.Unit("µg")
      case 30 => ajr.Unit("µg")
      case 32 => ajr.Unit("mg")
      case 33 => ajr.Unit("µg")
      case 34 => ajr.Unit("mg")
      case 36 => ajr.Unit("µg")
      case 37 => ajr.Unit("mg")
      case 38 => ajr.Unit("mg")
      case 39 => ajr.Unit("mg")
      case 40 => ajr.Unit("mg")
      case 41 => ajr.Unit("mg")
      case 42 => ajr.Unit("mg")
      case 43 => ajr.Unit("µg")
      case 44 => ajr.Unit("mg")
      case 45 => ajr.Unit("mg")
      case _ => throw new Exception("I honestly can't say :'(")
    }
  }

  def getProductColumnNameFromAjrId(ajrId: Int): String = {
    ajrId match {
      case 14 => "Retinol (µg)"
      case 21 => "Thiamin (mg)"
      case 22 => "Riboflavin (mg)"
      case 23 => "Niacin (mg)"
      case 24 => "Vitamin B - 6 (mg)"
      case 30 => "Vitamin B - 12 (µg)"
      case 32 => "Vitamin C (mg)"
      case 33 => "Vitamin D (D2 + D3) (µg)"
      case 34 => "Vitamin E (alpha - tocopherol) (mg)"
      case 36 => "Vitamin K (phylloquinone) (µg)"
      case 25 => "Folic acid (µg)"
      case 37 => "Calcium (mg)"
      case 38 => "Phosphorus (mg)"
      case 40 => "\"Iron (mg)\""
      case 44 => "Potassium (mg)"
      case 42 => "Copper (mg)"
      case 39 => "Magnesium (mg)"
      case 45 => "Sodium (mg)"
      case 43 => "Selenium (µg)"
      case 41 => "\"Zinc (mg)\""
      case _ => throw new Exception("oh no")
    }
  }
}
