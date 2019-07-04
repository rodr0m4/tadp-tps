package parser

object Musiquita {

  val silencio : Parser[Silencio] = char('_').const(Silencio(Blanca)) <|> char('-').const(Silencio(Negra)) <|> char('~').const(Silencio(Corchea))
  val sonido : Parser[Sonido] = numero <> nota <> figura
  val figura : Parser[Figura] = ???
  val nota : Parser[Nota] = charSatisfies
  private val numero : Parser[Int] = digit.map(_.toString.toInt)
}

sealed trait Tocable {
  val figura : Figura = ???
}

case class Silencio(override val figura : Figura) extends Tocable {

}


sealed trait Figura {

}

object Negra extends Figura {

}

object Blanca extends Figura {

}

object Corchea extends Figura {

}

case class Sonido(override val figura : Figura) extends Tocable {

}


trait Nota

case object C extends Nota
case object D extends Nota
case object E extends Nota
case object F extends Nota
case object G extends Nota
case object A extends Nota
case object B extends Nota

