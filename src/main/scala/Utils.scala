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
    throw new Exception("more than one char in the string")
  } else {
    str.toCharArray.head.toString
  }
}

case object CsvFloat extends CsvType {
  override def canParse(str: String): String = str.toDouble.toString
}

class Utils {

  def validateCsvType(str: String, option: Option[Seq[CsvType]], i: Int): String = {
    option match {
      case Some(tab) =>
        tab(i).canParse(str)
    }
  }


  /**
   *
   *
   *
   * @param file_path
   * @param delimiter
   * @param containsHeader
   * @param columnTypes
   * @return
   */
  def csvExtract(file_path: String,
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
    header match {
      case Some(h) =>
        if (h.length != h.distinct.length) {
          throw new Exception("The header contains non unique column name")
        }
    }

    // check the consistency of number of columns in header and `columnTypes` parameter
    (header, columnTypes) match {
      case (Some(h), Some(ct)) => if (h.length != ct.length) {
        throw new Exception("The header and the param `columnTypes` do not contains the same number of columns")
      }
    }

    // set up the count of lines to header's length or to column types length
    // this is a mutable variable because there is a possibility that neither header nor columnTypes are defined here
    // so this counter will be initialized while parsing first data line of file
    var count = header.map(_.length).orElse(columnTypes.map(_.length))

    val data = source
      // get the lines of the file
      .getLines()
      // zip them with an index to manipulate tuples (line, index)
      .zipWithIndex
      // remove first line if there was a header
      .filter { case (_, n) => !containsHeader || n > 0 }
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
  } // end of `csvExtract` function
} // end of `Utils` class

// objective : use inheritance
class CustomException(private val message: String = "",
                      private val cause: Throwable = None.orNull
                     ) extends Exception(message, cause)
