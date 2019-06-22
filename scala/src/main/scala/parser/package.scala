import scala.util.Try

package object parser {
  type Result[A] = Try[(A, String)]
}
