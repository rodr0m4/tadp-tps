import scala.util.{Failure, Success, Try}

package object parser {
  type Result[+A] = Try[(A, String)]

  val id: Parser[Any] = new Parser[Any] {
    override def apply(input: String): Result[Any] = Success((), input)
  }

  /*anyChar: lee cualquier caracter de un texto,
   si a este parser le paso el texto “hola”, debería devolver un parseo exitoso con el carácter h como resultado.
   Sin embargo, si le paso un texto vacío (“”), debería fallar.*/

  object anyChar extends Parser[Char] {
    override def apply(input: String): Result[Char] = input match {
      case "" => Failure(EmptyStringException)
      case _ => Success((input.head, input.tail))
    }
  }

  /*char: nos permite decir qué carácter esperamos encontrar.
  ejemplo, queremos poder tener un parser del char ‘c’, al cuál si le paso como parámetro el texto “chau”,
  da un resultado exitoso con el valor ‘c’, pero si le paso el texto “hola”, debería fallar porque “hola” no empieza con c. */

  def char(expected: Char): Parser[Char] = anyChar.satisfies(_ == expected, ExpectedButFound(expected, _))

  /*void: lee un carácter pero lo descarta. Lo que esperamos obtener como resultado de void es el valor Unit.
   Entonces, si lo usasemos para parsear “hola”, devolvería un resultado exitoso con Unit como valor.
   Pero ojo, si le pasasemos un string vacio “”, fallaría como cualquiera de los parsers anteriores.*/

  val void: Parser[Unit] = anyChar.const()

  /*letter: debería retornar un resultado exitoso para cualquier caracter que sea una letra (minúscula o mayúscula).
  Por ejemplo, si se usa para parsear “hola”, debería parsear ‘h’, pero si se usa para parsear “1234”, debería fallar.*/
  val letter: Parser[Char] = anyChar.satisfies(_.isLetter, NotALetterException)

  /*digit: debería devolver un resultado exitoso con el caracter parseado, si el carácter consumido es 0, 1, 2, 3, 4, 5, 6, 7, 8 o 9.*/
  val digit: Parser[Char] = anyChar.satisfies(_.isDigit, NotADigitException)

  val alphaNum: Parser[Char] = (digit <|> letter).mapError {
    case OrException(List(NotADigitException(_), NotALetterException(character))) => NotAlphaNumException(character)
    case e => e
  }

  /*string: este caso es parecido a char, porque necesita saber qué string es el que esperamos parsear,
   pero se diferencia a los parsers que aparecieron hasta ahora porque consume tantos caracteres como tenga el string esperado,
   y en el caso de éxito no debería tener un carácter como resultado, si no un string.
  Por ejemplo, queremos poder obtener un parser con el string esperado “hola”, tal que si lo usamos para parsear
  “hola mundo!”, de un resultado exitoso con el valor “hola”, pero si lo usasemos para intentar parsear “holgado” falle.
  */
  def string(expected: String): Parser[String] = expected.foldLeft(id)(_ <> char(_)).const(expected)

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


/*

[15:40, 11/7/2019] Ivan Puchalka UTN: Por cada caracter, lo hago un parser de char y lo concateno con <> con el resto
[15:41, 11/7/2019] Ivan Puchalka UTN: El problema era el tipo y la semilla
[15:41, 11/7/2019] Ivan Puchalka UTN: Como semilla uso id, que es un parser que hice que no toca el input
[15:41, 11/7/2019] Ivan Puchalka UTN: Pero es de tipo Parser[Any]
[15:42, 11/7/2019] Ivan Puchalka UTN: El resultado se vuelve de ese tipo
[15:42, 11/7/2019] Ivan Puchalka UTN: Pero como le hago const de un string, termina siendo Parser[String]
[15:43, 11/7/2019] Ivan Puchalka UTN: Todo muy lindo, pero es prácticamente un casteo
[15:43, 11/7/2019] Ivan Puchalka UTN: En otros contextos, que sea Any te caga todo
[15:44, 11/7/2019] Ivan Puchalka UTN: La solución es tunear más los tipos de id y del <>, probablemente*

*/