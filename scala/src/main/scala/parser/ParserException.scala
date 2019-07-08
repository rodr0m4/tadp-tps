package parser

class ParserException extends Exception

class NotExpectedInputException extends ParserException

//TODO: mensajes lindos
case class DoesNotSatisfyPredicateException[A](value: A) extends NotExpectedInputException

case class ExpectedButFound(expected: Char, found: Char) extends NotExpectedInputException

case object EmptyStringException extends ParserException

case class NotALetterException(actual: Char) extends NotExpectedInputException

case class NotADigitException(actual: Char) extends NotExpectedInputException

case class NotAlphaNumException(actual: Char) extends NotExpectedInputException

case class DoesNotStartWithException(prefix: String, actual: String) extends NotExpectedInputException {
  override def getMessage: String = s"the prefix is $prefix and the actual is $actual"
}

case class OrException(exceptions: List[Throwable]) extends NotExpectedInputException

object OrException {
  def apply(left: Throwable, right: Throwable): OrException = left match {
    case OrException(exceptions) => OrException(exceptions :+ right)
    case _ => OrException(List(left, right))
  }
}
