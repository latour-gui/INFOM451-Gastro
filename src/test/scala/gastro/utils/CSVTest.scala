package gastro.utils

import org.scalatest.FunSuite

import scala.util.Failure

// objective : (bonus) write unit tests
class CSVTest extends FunSuite {
  test("CSV.extract : coma with header") {
    val filePath = this.getClass.getResource("/good_coma_with_header.csv").getPath
    val data = CSV.extract(filePath, delimiter = ",")

    assert(data.isSuccess)
    assert(data.get.length == 3)
  }

  test("CSV.extract : coma without header") {
    val filePath = this.getClass.getResource("/good_coma_without_header.csv").getPath
    val data = CSV.extract(filePath, delimiter = ",", containsHeader = false)

    assert(data.isSuccess)
    assert(data.get.length == 3)
  }

  test("CSV.extract : semicolon with header") {
    val filePath = this.getClass.getResource("/good_semicolon_with_header.csv").getPath
    val data = CSV.extract(filePath)

    assert(data.isSuccess)
    assert(data.get.length == 3)
  }

  test("CSV.extract : semicolon without header") {
    val filePath = this.getClass.getResource("/good_semicolon_without_header.csv").getPath
    val data = CSV.extract(filePath, containsHeader = false)

    assert(data.isSuccess)
    assert(data.get.length == 3)
  }

  test("CSV.extract : more element in header") {
    val filePath = this.getClass.getResource("/bad_more_column_in_header.csv").getPath
    val data = CSV.extract(filePath, containsHeader = false)

    assert(data.isFailure)
    data match {
      case Failure(exception) => assert(exception.getMessage.contains("The number of columns is not consistent"))
    }
  }

  test("CSV.extract : more element in content") {
    val filePath = this.getClass.getResource("/bad_less_column_in_header.csv").getPath
    val data = CSV.extract(filePath, containsHeader = false)

    assert(data.isFailure)
    data match {
      case Failure(exception) => assert(exception.getMessage.contains("The number of columns is not consistent"))
    }
  }

  test("CSV.extract : with header, row length inconsistent") {
    val filePath = this.getClass.getResource("/bad_column_inconsistency_with_header.csv").getPath
    val data = CSV.extract(filePath, containsHeader = false)

    assert(data.isFailure)
    data match {
      case Failure(exception) => assert(exception.getMessage.contains("The number of columns is not consistent"))
    }
  }

  test("CSV.extract : without header, row length inconsistent") {
    val filePath = this.getClass.getResource("/bad_column_inconsistency_without_header.csv").getPath
    val data = CSV.extract(filePath, containsHeader = false)

    assert(data.isFailure)
    data match {
      case Failure(exception) => assert(exception.getMessage.contains("The number of columns is not consistent"))
    }
  }

  test("CSV.extract : duplicate header name") {
    val filePath = this.getClass.getResource("/bad_duplicate_header_name.csv").getPath
    val data = CSV.extract(filePath)

    assert(data.isFailure)
    data match {
      case Failure(exception) => assert(exception.getMessage.contains("The header contains non unique column name"))
    }
  }

  test("CSV.extract : column validation working") {
    val columnTypes: Seq[CsvType] = Seq(CsvChar, CsvFloat, CsvInt, CsvStr)
    val filePath = this.getClass.getResource("/respect_types.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isSuccess)
    assert(data.get.length == 4)
  }

  test("CSV.extract : column validation is wrong") {
    val columnTypes: Seq[CsvType] = Seq(CsvFloat, CsvChar, CsvInt, CsvStr)
    val filePath = this.getClass.getResource("/respect_types.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is good (int)") {
    val columnTypes: Seq[CsvType] = Seq(CsvInt, CsvInt)
    val filePath = this.getClass.getResource("/respect_types_int.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isSuccess)
    assert(data.get.length == 4)
  }

  test("CSV.extract : column validation is good (char)") {
    val columnTypes: Seq[CsvType] = Seq(CsvChar, CsvChar)
    val filePath = this.getClass.getResource("/respect_types_char.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isSuccess)
    assert(data.get.length == 4)
  }

  test("CSV.extract : column validation is good (float)") {
    val columnTypes: Seq[CsvType] = Seq(CsvFloat, CsvFloat)
    val filePath = this.getClass.getResource("/respect_types_float.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isSuccess)
    assert(data.get.length == 4)
  }


  test("CSV.extract : column validation is wrong (float to int)") {
    val columnTypes: Seq[CsvType] = Seq(CsvInt, CsvInt)
    val filePath = this.getClass.getResource("/respect_types_float.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (char to int)") {
    val columnTypes: Seq[CsvType] = Seq(CsvInt, CsvInt)
    val filePath = this.getClass.getResource("/respect_types_char.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (string to int)") {
    val columnTypes: Seq[CsvType] = Seq(CsvInt, CsvInt)
    val filePath = this.getClass.getResource("/respect_types_string.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (int to float)") {
    val columnTypes: Seq[CsvType] = Seq(CsvFloat, CsvFloat)
    val filePath = this.getClass.getResource("/respect_types_int.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isSuccess)
    assert(data.get.length == 4)
  }

  test("CSV.extract : column validation is wrong (char to float)") {
    val columnTypes: Seq[CsvType] = Seq(CsvFloat, CsvFloat)
    val filePath = this.getClass.getResource("/respect_types_char.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (string to float)") {
    val columnTypes: Seq[CsvType] = Seq(CsvFloat, CsvFloat)
    val filePath = this.getClass.getResource("/respect_types_string.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (float to char)") {
    val columnTypes: Seq[CsvType] = Seq(CsvChar, CsvChar)
    val filePath = this.getClass.getResource("/respect_types_float.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (int to char)") {
    val columnTypes: Seq[CsvType] = Seq(CsvChar, CsvChar)
    val filePath = this.getClass.getResource("/respect_types_int.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

  test("CSV.extract : column validation is wrong (string to char)") {
    val columnTypes: Seq[CsvType] = Seq(CsvChar, CsvChar)
    val filePath = this.getClass.getResource("/respect_types_string.csv").getPath
    val data = CSV.extract(filePath, columnTypes = Some(columnTypes))

    assert(data.isFailure)
    data match {
      case Failure(exception) => intercept[IllegalArgumentException](throw exception)
    }
  }

}
