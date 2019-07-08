package parser
import scala.util.{Failure, Success}

trait Parser[+A] extends (String => Result[A]) {self =>

  def apply(input: String): Result[A]

  def const[B](b: B): Parser[B] = map(_ => b)

  def map[B](function: A => B): Parser[B] = new Parser[B] {
    override def apply(input: String): Result[B] =
      self(input).map { case (value, remaining) => (function(value), remaining) }
  }

  def mapError(f: Throwable => Throwable): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).recoverWith({ case error => Failure(f(error)) })
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

  def satisfies(condition: A => Boolean, errorMessage: A => Exception = DoesNotSatisfyPredicateException(_)): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).flatMap { case result@(value, _) =>
      if (condition(value)) Success(result) else Failure(errorMessage(value))
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

      this (remaining).map { case (vs, rm) => (value :: vs, rm) }
    }
  }
  lazy val `+`: Parser[List[A]] = (this <> this.*).map { case head ~ tail => head :: tail }

  def sepBy[B](separator: Parser[B]): Parser[List[A]] =
    this <> (separator ~> this).* map {
      case head ~ tail => head :: tail
    }
}


