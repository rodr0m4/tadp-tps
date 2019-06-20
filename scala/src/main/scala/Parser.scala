trait Parser[A] {
  def apply(input: String):Result[A]
}

object anyChar extends Parser[Char] {
  override def apply(input: String): Result[Char] = input match {
  case "" => Failure("empty string")
  case _ => Success(input.head, input.tail)
}
}