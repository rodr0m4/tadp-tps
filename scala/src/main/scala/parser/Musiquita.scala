package parser

object Musiquita {
  lazy val sonido: Parser[Sonido] = numero <> nota <> figura map Sonido
  lazy val acordeExplicito: Parser[Acorde] = sonido.sepBy('-') <> figura map Acorde
//  lazy val acordeTonal: Parser[Acorde] = sonido <> oneOf(
//    'm' -> Menor,
//    'M' -> Mayor
//  )

  val silencio: Parser[Silencio] = oneOf(
    '_' -> Blanca,
    '-' -> Negra,
    '~' -> Corchea
  ) map Silencio

  val figura: Parser[Figura] = string("1/") ~> oneOf(
    '1' -> Redonda,
    '2' -> Blanca,
    '4' -> Negra,
    '8' -> Corchea,
    "16" -> SemiCorchea
  )

  val nota: Parser[Nota] = oneOf(
    'A' -> A,
    'B' -> B,
    'C' -> C,
    'D' -> D,
    'E' -> E,
    'F' -> F,
    'G' -> G
  )

  private val numero : Parser[Int] = digit.map(_.toString.toInt)
}

sealed trait Tocable {
  val figura : Figura
}

case class Silencio(override val figura : Figura) extends Tocable {

}

sealed trait Tonalidad
case object Menor extends Tonalidad
case object Mayor extends Tonalidad

sealed trait Figura
object Redonda extends Figura
object Negra extends Figura
object Blanca extends Figura
object Corchea extends Figura
object SemiCorchea extends Figura

case class Sonido(tono: Int, nota: Nota, override val figura : Figura) extends Tocable
case class Acorde(sonidos: List[Sonido], override val figura: Figura) extends Tocable

sealed trait Nota

case object C extends Nota
case object D extends Nota
case object E extends Nota
case object F extends Nota
case object G extends Nota
case object A extends Nota
case object B extends Nota

