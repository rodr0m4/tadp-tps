package parser

import scala.util.{Failure, Success}

trait Parser[+A] extends ((String) => Result[A]) { self =>
  def apply(input: String): Result[A]

  def const[B](b: B): Parser[B] = map(_ => b)

  def map[B](function: A => B): Parser[B] = new Parser[B] {
    override def apply(input: String): Result[B] =
      self(input).map { case (value, remaining) => (function(value), remaining) }
  }

  def <|>[B >: A](parser: Parser[B]): Parser[B] = new Parser[B] {
    override def apply(input: String): Result[B] = self(input).orElse(parser(input))
  }

  def <>[B](parser: Parser[B]): Parser[~[A, B]] = new Parser[~[A, B]] {
    override def apply(input: String): Result[~[A, B]] = for {
      (firstValue, firstRemaining) <- self(input)
      (secondValue, secondRemaining) <- parser(firstRemaining)
    } yield (firstValue ~ secondValue, secondRemaining)
  }

  def ~>[B](parser: Parser[B]): Parser[B] = (this <> parser).map(_._2)

  def <~[B](parser: Parser[B]): Parser[A] = (this <> parser).map(_._1)

  def satisfies(condition: A => Boolean): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).flatMap { case result@(value, _) =>
      if (condition(value)) Success(result) else Failure(DoesNotSatisfyPredicateException(value))
    }
  }

  lazy val opt: Parser[Option[A]] = new Parser[Option[A]] {
    override def apply(input: String): Result[Option[A]] =
      self(input).map {
        case (value, remaining) => (Some(value), remaining)
      }.recover { case _ => (None, input) }
  }

  lazy val `?`: Parser[Option[A]] = opt
  lazy val `*`: Parser[List[A]] = new Parser[List[A]] {
    override def apply(input: String): Result[List[A]] = {
      val result = self(input)

      if (result.isFailure) return Success((List(), input))

      val (value, remaining) = result.get

      this(remaining).map { case (vs, rm) => (value :: vs, rm) }
    }
  }
  lazy val `+`: Parser[List[A]] = (this <> this.*).map { case head ~ tail => head :: tail }

  def sepBy[B](separator: Parser[B]): Parser[List[A]] =
    this <> (separator ~> this).* map {
      case head ~ tail => head :: tail
    }
}

case class DoesNotSatisfyPredicateException[A](value: A) extends Exception

object anyChar extends Parser[Char] {
  override def apply(input: String): Result[Char] = charSatisfies(_ => true)(input)
}

object char {

  case class ExpectedButFound(expected: Char, found: Char) extends Exception

  def apply(expected: Char): Parser[Char] = charSatisfies(_ == expected, ExpectedButFound(expected, _))
}

case object EmptyStringException extends Exception


object charSatisfies {

  def apply(condition: Char => Boolean,
            errorMessage: Char => Exception = x => new Exception(s"$x did not match")): Parser[Char] = new Parser[Char] {
    override def apply(input: String): Result[Char] = input match {
      case "" => Failure(EmptyStringException)
      case i if condition(i.head) => Success(i.head, i.tail)
      case _ => Failure(errorMessage(input.head))
    }
  }
}

//Preguntar lo que tiene que devolver esta función: Unit type?
object void extends Parser[Unit] {
  override def apply(input: String): Result[Unit] = anyChar.const()(input)
}

object letter extends Parser[Char] {

  case class NotALetterException(actual: Char) extends Exception

  override def apply(input: String): Result[Char] =
    charSatisfies(_.isLetter, NotALetterException)(input)
}

object digit extends Parser[Char] {

  case class NotADigitException(actual: Char) extends Exception

  override def apply(input: String): Result[Char] =
    charSatisfies(_.isDigit, NotADigitException)(input)
}

object alphaNum extends Parser[Char] {

  case class NotAlphaNumException(actual: Char) extends Exception

  override def apply(input: String): Result[Char] =
    charSatisfies(_.isLetterOrDigit, NotAlphaNumException)(input)
}

object string {

  case class DoesNotStartWithException(prefix: String, actual: String) extends Exception {
    override def getMessage: String = s"the prefix is $prefix and the actual is $actual"
  }

  def apply(prefix: String): Parser[String] = new Parser[String] {
    override def apply(input: String): Result[String] =
      if (input.startsWith(prefix))
        Success(prefix, input.replaceFirst(prefix, ""))
      else
        Failure(DoesNotStartWithException(prefix, input))
  }
}

object oneOf {
  def apply[A](parsers: Parser[A]*): Parser[A] = parsers.reduce(_ <|> _)
}