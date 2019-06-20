import org.scalactic.ErrorMessage

trait Parser[A] { self =>
  def apply(input: String):Result[A]

  def const[B] (b:B): Parser[B] = map(_ => b)

  def map[B](function: A =>B) : Parser[B] = new Parser[B]{
    override def apply(input: String): Result[B] = self(input) match {
      case success : Success[A]=> success.copy(value = function(success.value))
      case failure : Failure => failure
    }
  }
}

object anyChar extends Parser[Char] {
  override def apply(input: String): Result[Char] = charSatisfies(_ => true)(input)
}

object char {
  def apply(expected: Char) : Parser[Char] = charSatisfies(_ == expected, actual => s"$actual is not $expected")
}

object charSatisfies {
  def apply(condition : Char => Boolean, errorMessage: Char => String = x => s"$x did not match") : Parser[Char] = new Parser[Char] {
    override def apply(input: String): Result[Char] = input match {
      case "" => Failure("empty string")
      case x if condition(x.head) => Success(input.head, input.tail)
      case _ => Failure(errorMessage(input.head))
    }
  }
}

//Preguntar lo que tiene que devolver esta función: Unit type?
object void extends Parser[Unit]{
  override def apply(input: String): Result[Unit] = anyChar.const()(input)
}

object letter extends Parser[Char]{
  override def apply(input: String): Result[Char] = charSatisfies(_.isLetter, actual => s"$actual is not letter")(input)
}

object digit extends Parser[Char]{
  override def apply(input: String): Result[Char] = charSatisfies(_.isDigit, actual => s"$actual is not digit")(input)
}

object alphaNum extends Parser[Char] {
  override def apply(input: String): Result[Char] = charSatisfies(_.isLetterOrDigit, actual => s"$actual is neither letter nor digit")(input)
}