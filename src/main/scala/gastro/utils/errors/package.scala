package gastro.utils

package object errors {

  // objective : use inheritance
  class CustomException(private val message: String = "", private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  class HeaderException(private val message: String = "", private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  class StringToCharParsingException(private val message: String = "", private val cause: Throwable = None.orNull)
    extends IllegalArgumentException(message, cause)

}
