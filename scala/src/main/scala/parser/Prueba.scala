package parser

case object Prueba extends App {

  def coolSyntax: Parser[Number] = (digit.+ <> (char('.') <> digit.+).?).map {
    case integralPart ~ None => integralPart.mkString.toInt
    case integralPart ~ Some(dot ~ fractionalPart) =>
      (integralPart.toString + dot.toString + fractionalPart.toString).toDouble
  }

  def doesNotWorkInTests: Parser[Double] = string("val") <> letter.+ <> char('=') <> digit.+ map {
    case _ ~ identifier ~ _ ~ expr => (identifier ++ "." ++ expr).mkString.toInt
  }

  def materia: String = "parser"
}
