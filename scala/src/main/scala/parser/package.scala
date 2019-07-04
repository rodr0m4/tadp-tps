import scala.util.Try

package object parser {
  type Result[+A] = Try[(A, String)]

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

  implicit def flatten2[A, B, C]         (f: (A, B) => C) =
    (p: ~[A, B]) => p match {case a ~ b => f(a, b)}
  implicit def flatten3[A, B, C, D]      (f: (A, B, C) => D) =
    (p: ~[~[A, B], C]) => p match {case a ~ b ~ c => f(a, b, c)}
  implicit def flatten4[A, B, C, D, E]   (f: (A, B, C, D) => E) =
    (p: ~[~[~[A, B], C], D]) => p match {case a ~ b ~ c ~ d => f(a, b, c, d)}
  implicit def flatten5[A, B, C, D, E, F](f: (A, B, C, D, E) => F) =
    (p: ~[~[~[~[A, B], C], D], E]) => p match {case a ~ b ~ c ~ d ~ e=> f(a, b, c, d, e)}
}
