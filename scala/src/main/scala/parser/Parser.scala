package parser

import scala.util.{Failure, Success}

trait Parser[+A] extends (String => Result[A]) {
  self =>

  def apply(input: String): Result[A]

  /*const: recibe un valor y convierte a un parser en otro diferente que parsea de la misma manera
  (falla cuando fallaría el original, funciona cuando funcionaría el original)
   pero que como elemento parseado devuelve el valor constante que se le pasó a const,
   en vez de lo que devolvería el parser base.*/
  def const[B](b: B): Parser[B] = map(_ => b)

  /*map: dada una función de transformación y un parser, retorna un nuevo parser que parsea lo mismo que el original
  y convierte el valor parseado utilizando la función recibida.
   */
  def map[B](function: A => B): Parser[B] = new Parser[B] {
    override def apply(input: String): Result[B] =
      self(input).map { case (value, remaining) => (function(value), remaining) }
  }

  def mapError(f: Throwable => Throwable): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).recoverWith({ case error => Failure(f(error)) })
  }

  /*OR: a partir de dos parsers crea uno que trabaja de la siguiente manera: si el primer parser retorna un resultado,
  ese es el resultado, si no, intenta con el segundo parser y retorna lo que retornaría este último.

  val aob = char('a') <|> char('b')
  Si le pasamos “arbol” a aob, debería poder parsear ‘a’, y si le pasasemos “bort” a aob debería poder parsear ‘b’.*/

  def <|>[B >: A](parser: Parser[B]): Parser[B] = new Parser[B] {
    override def apply(input: String): Result[B] =
      self(input).recoverWith {
        case firstException: NotExpectedInputException =>
          parser(input).recoverWith {
            case secondException: NotExpectedInputException =>
              Failure(OrException(firstException, secondException))
            case e => throw e
          }
        case e => throw e
      }
  }

  /*CONCAT: un parser combinator que secuencia dos parsers, crea uno nuevo que primero parsea lo que parsearía el primero,
  y usando el resto del texto aplica el segundo parser. Esperamos que el valor que devuelva en caso exitoso tenga una tupla
  con los dos valores parseados.
  Por ejemplo:
  val holaMundo = string("hola") <> string("mundo")
  Si a holaMundo le pasasemos “holamundo”, debería producir un resultado exitoso con los valores “hola” y “mundo” en una tupla.
   Si le pasásemos “holachau”, debería fallar, porque o parsea todo o no parsea nada */

  def <>[B](parser: Parser[B]): Parser[~[A, B]] = new Parser[~[A, B]] {
    override def apply(input: String): Result[~[A, B]] = for {
      (firstValue, firstRemaining) <- self(input)
      (secondValue, secondRemaining) <- parser(firstRemaining)
    } yield (firstValue ~ secondValue, secondRemaining)
  }

  /*(primerParser ~> segundoParser) debería requerir que ambos parsers se hagan de manera secuencial
  (primero primerParser y luego segundoParser) pero me da sólo el resultado de segundoParser.*/
  def ~>[B](parser: Parser[B]): Parser[B] = (this <> parser).map(_._2)

  /*<~: (primerParser <~ segundoParser) parsea en el mismo orden que el anterior, pero me da el resultado de primerParser.*/
  def <~[B](parser: Parser[B]): Parser[A] = (this <> parser).map(_._1)

  /*satisfies: a partir de un parser y una condición,
    nos permite obtener un parser que funciona sólo si el parser base funciona
    y además el elemento parseado cumple esa condición.*/
  def satisfies(condition: A => Boolean, errorMessage: A => Exception = DoesNotSatisfyPredicateException(_)): Parser[A] = new Parser[A] {
    override def apply(input: String): Result[A] = self(input).flatMap { case result@(value, _) =>
      if (condition(value)) Success(result) else Failure(errorMessage(value))
    }
  }

  /*OPT: convierte en opcional a un parser. Es decir, el nuevo parser siempre debería dar un resultado exitoso,
  pero si el parser original hubiese fallado, el resultado no contendrá ningún valor y no consumirá ningún caracter del input.
  Ejemplo:
  val talVezIn = string("in").opt
  // precedencia parsea exitosamente las palabras "infija" y "fija"
  val precedencia = talVezIn <> string("fija")
  Si a precedencia le pasasemos “fija”, deberia devolver una tupla con un valor vacío y con el valor “fija”,
  porque talVezIn no habría consumido ningún carácter del texto original.
  */

  lazy val opt: Parser[Option[A]] = new Parser[Option[A]] {
    override def apply(input: String): Result[Option[A]] =
      self(input).map {
        case (value, remaining) => (Some(value), remaining)
      }.recover { case _ => (None, input) }
  }

  lazy val `?`: Parser[Option[A]] = opt

/* la clausura de Kleene se aplica a un parser, convirtiéndolo en otro que se puede aplicar todas las veces que sea posible o 0 veces.
 El resultado debería ser una lista que contiene todos los valores que hayan sido parseados (podría no haber ninguno).
*/
  lazy val `*`: Parser[List[A]] = new Parser[List[A]] {
    override def apply(input: String): Result[List[A]] = {
      val result = self(input)

      if (result.isFailure) return Success((List(), input))

      val (value, remaining) = result.get

      this (remaining).map { case (vs, rm) => (value :: vs, rm) }
    }
  }

  /*+: es como la clausura de Kleene pero requiere que el parser se aplique al menos UNA vez.*/
  lazy val `+`: Parser[List[A]] = (this <> this.*).map { case head ~ tail => head :: tail }

  /*sepBy: toma dos parsers: un parser de contenido y un parser separador, parsea 1 o más veces el parser de contenido
  (similar a la cláusula de kleene+) pero entre cada una aplica el parser separador.
  Ejemplo:
  val numeroDeTelefono = integer.sepBy(char('-'))
  Integer sería un parser aplica a 1 o más dígitos, y devuelve un Int con el valor del número representado por los dígitos.
  El parser resultado del sepBy debería funcionar si le paso “4356-1234” pero no si le paso “4356 1234”.
  Al parsear debería devolver una lista cuyos elementos serán el resultado de aplicar el parser de contenido a cada “grupo”.
  */
  def sepBy[B](separator: Parser[B]): Parser[List[A]] =
    (this <> (separator ~> this).*).map {
      case head ~ tail => head :: tail
    }
}


/*
  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like `flatMap` for the exception.
   */
  def recoverWith[U >: T](f: PartialFunction[Throwable, Try[U]]): Try[U]

  /**
   * Applies the given function `f` if this is a `Failure`, otherwise returns this if this is a `Success`.
   * This is like map for the exception.
   */
  def recover[U >: T](f: PartialFunction[Throwable, U]): Try[U]
*/

