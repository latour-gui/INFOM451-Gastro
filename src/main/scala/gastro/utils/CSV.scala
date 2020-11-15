package gastro.utils

import gastro.utils.errors.{HeaderException, StringToCharParsingException}

import scala.util.Try

abstract class CsvType {
  def canParse(str: String): String
}

case object CsvInt extends CsvType {
  override def canParse(str: String): String = Integer.parseInt(str).toString
}

case object CsvStr extends CsvType {
  override def canParse(str: String): String = str
}


case object CsvChar extends CsvType {
  override def canParse(str: String): String = if (str.toCharArray.length != 1) {
    throw new StringToCharParsingException("more than one char in the string")
  } else {
    str.toCharArray.head.toString
  }
}

case object CsvFloat extends CsvType {
  override def canParse(str: String): String = str.toDouble.toString
}

object CSV {

  /**
   * Check if the `str` given can be parsed as the type given by the `columnTypes(index)` CsvType.
   *
   * @param str         the string that have to be parsed
   * @param columnTypes the list of the column types
   * @param index       the index of the column that have to be tested
   * @return the input str
   * @throws IllegalArgumentException if its not possible to parse the str
   */
  def validateCsvType(str: String, columnTypes: Option[Seq[CsvType]], index: Int): String = {
    columnTypes match {
      case Some(tab) =>
        tab(index).canParse(str)
    }
    str
  }


  /**
   * Extract information from a CSV file.
   * The output format is a sequence of Map(key,value) as (column name, value).
   * Each Map is a row.
   *
   * @param file_path      the csv file that needs to be extracted
   * @param delimiter      the delimiter of the file (default = ";")
   * @param containsHeader whether the first line of the file is for the header
   * @param columnTypes    the column types for additional robustness
   * @return a list of maps containing the column name as key and the value as value
   */
  def extract(file_path: String,
              delimiter: String = ";",
              containsHeader: Boolean = true,
              columnTypes: Option[Seq[CsvType]] = None)
  : Try[Seq[Map[String, String]]]
  = Try {
    // get the file as a manipulable object
    val source = scala.io.Source.fromFile(file_path)

    // get the header as a list of string
    val header: Option[Seq[String]] = (if (containsHeader) {
      // Special thanks to leedm777 from https://stackoverflow.com/questions/8865434/how-to-get-first-line-from-file-in-scala
      source.getLines.find(_ => true)
    } else {
      None
    }).map(line => line.split(delimiter).map(r => r.trim))

    // check the uniqueness of the column names in the header
    if (header.isDefined && header.get.length != header.get.distinct.length) {
      throw new HeaderException("The header contains non unique column name")
    }

    // check the consistency of number of columns in header and `columnTypes` parameter
    if ((header.isDefined && columnTypes.isDefined) && header.get.length != columnTypes.get.length) {
      throw new HeaderException("The header and the param `columnTypes` do not contains the same number of columns")
    }

    // set up the count of lines to header's length or to column types length
    // this is a mutable variable because there is a possibility that neither header nor columnTypes are defined here
    // so this counter will be initialized while parsing first data line of file
    var count = header.map(_.length).orElse(columnTypes.map(_.length))

    // objective : master for comprehension
    val data = source
      // get the lines of the file
      .getLines()
      // zip them with an index to manipulate tuples (line, index)
      .zipWithIndex
      // remove first line if there was a header
      //      .filter { case (_, n) => !containsHeader || n > 0 }
      .map { case (line, line_number) =>
        // take every column
        val cols = line.split(delimiter)

        // check consistency of number of columns in file
        count match {
          case Some(v) => if (v != cols.length) {
            throw new Exception("The number of columns is not consistent (error at line " + line_number + ").")
          }
          case None => count = Some(cols.length)
        }

        // real transformations here
        cols.zipWithIndex.map {
          case (c, i) => (
            header.map(_ (i)).orElse(Some("col_" + i)).get,
            columnTypes.map(_ (i).canParse(c)).orElse(Some(c)).get
          )
        }.toMap

      }.toSeq

    // close the source
    source.close()

    // return data
    data
  }
}

