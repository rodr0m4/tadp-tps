package parser

import scala.util.{Failure, Success}

trait Parser[A] { self =>
  def apply(input: String): Result[A]

  def const[B](b: B): Parser[B] = map(_ => b)

  def map[B](function: A => B): Parser[B] = new Parser[B] {
    override def apply(input: String): Result[B] =
      self(input).map { case (value, remaining) => (function(value), remaining) }
  }

  def <|>(parser: Parser[A]): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).orElse(parser(input))
  }

  def <>[B](parser: Parser[B]): Parser[(A, B)] = new Parser[(A, B)] {
    override def apply(input: String): Result[(A, B)] = for {
      (firstValue, firstRemaining) <- self(input) //yo no quiero que sea este new parser sino eloriginal
      (secondValue, secondRemaining) <- parser(firstRemaining)
    } yield ((firstValue, secondValue), secondRemaining)
  }

  def ~>[B](parser: Parser[B]): Parser[B] = (this <> parser).map(_._2)

  def <~[B](parser: Parser[B]): Parser[A] = (this <> parser).map(_._1)

  def satisfies(condition: A => Boolean): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).flatMap { case result@(value, _) =>
      if (condition(value)) Success(result) else Failure(DoesNotSatisfyPredicateException(value))
    }
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

object charSatisfies {

  case object EmptyStringException extends Exception

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

  case class DoesNotStartWithException(prefix: String, actual: String) extends Exception

  def apply(prefix: String): Parser[String] = new Parser[String] {
    override def apply(input: String): Result[String] =
      if (input.startsWith(prefix))
        Success(prefix, input.replace(prefix, ""))
      else
        Failure(DoesNotStartWithException(prefix, input))
  }
}