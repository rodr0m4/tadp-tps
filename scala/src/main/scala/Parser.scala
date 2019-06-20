trait Parser[A] {
  def apply(input: String):Result[A]
}

object anyChar extends Parser[Char] {
  override def apply(input: String): Result[Char] = input match {
    case "" => Failure("empty string")
    case _ => Success(input.head, input.tail)
  }
}

object char {
  def apply(expected: Char) : Parser[Char] = new Parser[Char] {
    override def apply(input: String): Result[Char] = input match {
      case "" => Failure("empty string")
      case x if x.head == expected => Success(input.head, input.tail)
      case _ => Failure(s"${input.head} is not $expected")
    }
  }
}