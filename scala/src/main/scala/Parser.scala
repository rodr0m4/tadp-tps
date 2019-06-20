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

  def <>[B](parser: Parser[B]) : Parser[(A, B)] = new Parser[(A, B)] {
    override def apply(input: String): Result[(A, B)] = self(input) match {
      case Success(firstValue, remaining) => parser(remaining) match {
        case Success(secondValue, secondRemaining) => Success((firstValue, secondValue), secondRemaining)
        case failure : Failure => failure
      }
      case failure : Failure => failure
    }
  }

  def ~>[B](parser: Parser[B]) : Parser[B] = (this <> parser).map(_._2)

  def <~[B](parser: Parser[B]) : Parser[A] = (this <> parser).map(_._1)
}

object anyChar extends Parser[Char] {
  override def apply(input: String): Result[Char] = charSatisfies(_ => true)(input)
}

object char {
  def apply(expected: Char) : Parser[Char] = charSatisfies(_ == expected, actual => s"$actual is not $expected")
}

// -----------------

// IMPLEMENTACION MAS OBJETOSA
// PROS: no paso input por todos lados
// CONTRAS: tiene mas codigo, es mas largo

trait CharPredicate extends Parser[Char] {
  def matches(char: Char): Boolean
  def errorMessage(char: Char): String = s"$char did not match"

  def apply(input: String): Result[Char] = input match {
    case "" => Failure("empty string")
    case x if matches(x.head) => Success(input.head, input.tail)
    case _ => Failure(errorMessage(input.head))
  }
}

object anyChar2 extends CharPredicate {
  override def matches(char: Char): Boolean = true
}

object char2 {
  def apply(expected : Char) : CharPredicate = new CharPredicate {
    override def matches(char: Char): Boolean = expected == char

    override def errorMessage(char: Char): String = s"$char is not $expected"
  }

}

object letter2 extends CharPredicate {
  override def matches(char: Char): Boolean = char.isLetter

  override def errorMessage(char: Char): String = s"$char is not letter"
}

object digit2 extends CharPredicate {
  override def matches(char: Char): Boolean = char.isDigit

  override def errorMessage(char: Char): String = s"$char is not digit"
}

object alphaNum2 extends CharPredicate {
  override def matches(char: Char): Boolean = char.isLetterOrDigit

  override def errorMessage(char: Char): String = s"$char is neither letter nor digit"
}

// -----------------

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

object string {
  def apply(prefix: String): Parser[String] = new Parser[String] {
    override def apply(input: String): Result[String] = input match {
      case _ if input.startsWith(prefix) => Success(prefix, input.replace(prefix, ""))
      case _ => Failure(s"$input does not start with $prefix")
    }
  }
}