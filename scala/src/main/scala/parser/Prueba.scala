package parser

case object Prueba extends App {

  def asd: Parser[Number] = (digit.+ <> (char('.') <> digit.+).?).map {
    case integralPart ~ None => integralPart.mkString.toInt
    case integralPart ~ Some(dot ~ fractionalPart) =>
      (integralPart.toString + dot.toString + fractionalPart.toString).toDouble
  }

  def jjj = string("val") <> letter.+ <> char('=') <> digit.+ map {
    // This chain does not work in tests :(q
    case _ ~ identifier ~ _ ~ expr => (identifier, expr)
  }

  def materia: String = "parser"
}
