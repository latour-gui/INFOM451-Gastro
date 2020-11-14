package gastro.kitchen

import gastro.utils.CSV

import scala.util.Try

package object food {
  def lookForProduct(filePath: String, ajrs: Seq[ajr.Limit]): Try[Seq[Product]] =
    CSV.extract(filePath).map(seqMaps => for {
      headerValueMap <- seqMaps

      ajrsAsProductProperties = for {
        a <- ajrs
        value <- headerValueMap.get(ajr.getProductColumnNameFromAjrId(a.id)).flatMap(_.trim.toDoubleOption)
      } yield new ProductProperty(a.id, a.name, new ajr.Value(value, ajr.getUnitFromProductColId(a.id)))
      id <- headerValueMap.get("Food code").flatMap(_.trim.toIntOption)
      name <- headerValueMap.get("Main food description")
    } yield new Product(
      id,
      name,
      ajrsAsProductProperties))


  def rememberAJR(filePath: String): Try[Seq[ajr.Limit]] =
    CSV.extract(filePath).map(seqMaps => for {
      headerValueMap <- seqMaps

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
      id))
}
