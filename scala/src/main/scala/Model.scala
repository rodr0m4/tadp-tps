sealed trait Result[+A]
case class Success[A](value:A, remaining:String) extends Result[A]
case class Failure(reason:String) extends Result[Nothing]