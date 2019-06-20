sealed trait Result[+A]{
  //def map[B](function : A => B) : Result[B]

  //def flatMap[B](function: A => Result[B]) : Result[B]
}
case class Success[A](value:A, remaining:String) extends Result[A]
case class Failure(reason:String) extends Result[Nothing]