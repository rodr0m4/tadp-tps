import scala.util.Try

package object parser {
  type Result[+A] = Try[(A, String)]

  case class ~[+A, +B](_1: A, _2: B) {
    override def toString = s"(${_1}~${_2})"
  }

  implicit class TildeSyntax[A](a: A) {
    def ~[B](b: B): ~[A, B] = new ~(a, b)
  }
}
