package parser

class ParserException extends Exception
//TODO: mensajes lindos
case class DoesNotSatisfyPredicateException[A](value: A) extends ParserException
case class EmptyStringException(value : Char) extends ParserException
case class ExpectedButFound(expected: Char, found: Char) extends ParserException
case object EmptyStringException extends ParserException
case class NotALetterException(actual: Char) extends ParserException
case class NotADigitException(actual: Char) extends ParserException
case class NotAlphaNumException(actual: Char) extends ParserException
case class DoesNotStartWithException(prefix: String, actual: String) extends ParserException {
  override def getMessage: String = s"the prefix is $prefix and the actual is $actual"
}

case class OrException(exceptions: List[Throwable]) extends ParserException
object OrException {
  def apply(left: Throwable, right: Throwable): OrException = left match {
    case OrException(exceptions) => OrException(exceptions :+ right)
    case _                       => OrException(List(left, right))
  }
}
