import scala.util.{Failure, Success, Try}

package object parser {
  type Result[+A] = Try[(A, String)]

  object anyChar extends Parser[Char] {

    override def apply(input: String): Result[Char] = input match {
      case "" => Failure(EmptyStringException)
      case _ => Success((input.head, input.tail))
    }
  }

  def char(expected: Char): Parser[Char] = anyChar.satisfies(_ == expected, ExpectedButFound(expected, _))

  val void: Parser[Unit] = anyChar.const()

  val letter: Parser[Char] = anyChar.satisfies(_.isLetter, NotALetterException)

  val digit: Parser[Char] = anyChar.satisfies(_.isDigit, NotADigitException)

  val alphaNum: Parser[Char] = (digit <|> letter).mapError {
    case OrException(List(NotADigitException(_), NotALetterException(character))) => NotAlphaNumException(character)
    case e => e
  }

  case class string(prefix: String) extends Parser[String] {
    override def apply(input: String): Result[String] =
      if (input.startsWith(prefix))
        Success(prefix, input.replaceFirst(prefix, ""))
      else
        Failure(DoesNotStartWithException(prefix, input))
  }

  def oneOf[A](parsers: Parser[A]*): Parser[A] = parsers.reduce(_ <|> _)

  case class ~[+A, +B](_1: A, _2: B) {
    override def toString = s"(${_1}~${_2})"
  }

  implicit class TildeSyntax[A](a: A) {
    def ~[B](b: B): ~[A, B] = new ~(a, b)
  }

  private def convertToParser[A, B](parser: Parser[A], result: B): Parser[B] = parser.const(result)

  implicit def charPairToParser[A](pair: (Char, A)): Parser[A] = convertToParser(pair._1, pair._2)

  implicit def stringPairToParser[A](pair: (String, A)): Parser[A] = convertToParser(pair._1, pair._2)

  implicit def charToParser(c: Char): Parser[Char] = char(c)

  implicit def stringToParser(s: String): Parser[String] = string(s)





























  implicit def flatten2[A, B, C](f: (A, B) => C) =
    (p: ~[A, B]) => p match {
      case a ~ b => f(a, b)
    }

  implicit def flatten3[A, B, C, D](f: (A, B, C) => D) =
    (p: ~[~[A, B], C]) => p match {
      case a ~ b ~ c => f(a, b, c)
    }

  implicit def flatten4[A, B, C, D, E](f: (A, B, C, D) => E) =
    (p: ~[~[~[A, B], C], D]) => p match {
      case a ~ b ~ c ~ d => f(a, b, c, d)
    }

  implicit def flatten5[A, B, C, D, E, F](f: (A, B, C, D, E) => F) =
    (p: ~[~[~[~[A, B], C], D], E]) => p match {
      case a ~ b ~ c ~ d ~ e => f(a, b, c, d, e)
    }
}
