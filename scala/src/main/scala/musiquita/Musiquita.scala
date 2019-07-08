package musiquita

import musiquita.Musiquita.Octava
import parser._

object Musiquita {
  type Octava = Int

  lazy val melodia: Parser[Melodia] = tocable.sepBy(' ')
  lazy val tocable: Parser[Tocable] = oneOf(silencio, sonido, acorde)
  lazy val tono: Parser[Tono] = numero <> nota map Tono
  lazy val sonido: Parser[Sonido] = tono <> figura map Sonido
  lazy val acorde: Parser[Acorde] = acordeExplicito <|> acordeTonal
  lazy val acordeExplicito: Parser[Acorde] = tono.sepBy('+') <> figura map Acorde
  lazy val acordeTonal: Parser[Acorde] = tono <> mayorOMenor <> figura map {
    case Tono(octava, nota) ~ mayorOMenor ~ figura =>
      mayorOMenor(nota)(octava, figura)
  }

  lazy val mayorOMenor: Parser[Nota => (Octava, Figura) => Acorde] = oneOf(
    'm' -> { nota: Nota => nota.acordeMenor _ },
    'M' -> { nota: Nota => nota.acordeMayor _ }
  )

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
    'C' -> C,
    'D' -> D,
    'E' -> E,
    'F' -> F,
    'G' -> G,
    'A' -> A,
    'B' -> B
  ) <> oneOf('#' -> Sostenido, 'b' -> Bemol).?.map(_.getOrElse(SinAccidente)) map {
    // TODO: Deberíamos fallar en estos casos?
    case B ~ Sostenido => C()
    case C ~ Bemol => B()
    case E ~ Sostenido => F()
    case F ~ Bemol => E()

    case nota ~ accidente => nota(accidente)
  }

  private val numero: Parser[Int] = digit.map(_.toString.toInt)
}

sealed trait Accidente

case object SinAccidente extends Accidente

case object Sostenido extends Accidente

case object Bemol extends Accidente

sealed trait Nota {
  val accidente: Accidente

  lazy val Sostenido: Nota = Nota.idToNota(this.toId + 1)
  lazy val Bemol: Nota = Nota.idToNota(this.toId - 1)

  def acordeMenor(octava: Octava = 0, figura: Figura): Acorde =
    Acorde((this :: this + 3 :: this + 7 :: Nil).map(Tono(octava, _)), figura)

  def acordeMayor(octava: Octava = 0, figura: Figura): Acorde =
    Acorde((this :: this + 4 :: this + 7 :: Nil).map(Tono(octava, _)), figura)

  def +(cantidadDeSemitonos: Int): Nota = 1.to(cantidadDeSemitonos).foldLeft(this) {
    case (nota, _) => nota.Sostenido
  }

  private def toId: Int = Nota.notaToId(this)
}

object Nota {
  private val notaToIdTable: Map[Nota, Int] = Map(
    C(Bemol) -> 11, // TODO: Sacar?
    C() -> 0,
    C(Sostenido) -> 1,
    D(Bemol) -> 1,
    D() -> 2,
    D(Sostenido) -> 3,
    E(Bemol) -> 3,
    E() -> 4,
    F(Bemol) -> 4, // TODO: Sacar?
    E(Sostenido) -> 5, // TODO: Sacar?
    F() -> 5,
    F(Sostenido) -> 6,
    G(Bemol) -> 6,
    G() -> 7,
    G(Sostenido) -> 8,
    A(Bemol) -> 8,
    A() -> 9,
    A(Sostenido) -> 10,
    B() -> 11,
    B(Sostenido) -> 0 // TODO: Sacar?
  )

  private val idToNotaTable: Map[Int, Nota] = Map(
    0 -> C(),
    1 -> C(Sostenido),
    2 -> D(),
    3 -> D(Sostenido),
    4 -> E(),
    5 -> F(),
    6 -> F(Sostenido),
    7 -> G(),
    8 -> G(Sostenido),
    9 -> A(),
    10 -> A(Sostenido),
    11 -> B()
  )

  def notaToId(nota: Nota): Int = notaToIdTable.getOrElse(nota, 0)

  def idToNota(id: Int): Nota = idToNotaTable.getOrElse(id, C())
}

case class C(override val accidente: Accidente = SinAccidente) extends Nota

case class D(override val accidente: Accidente = SinAccidente) extends Nota

case class E(override val accidente: Accidente = SinAccidente) extends Nota

case class F(override val accidente: Accidente = SinAccidente) extends Nota

case class G(override val accidente: Accidente = SinAccidente) extends Nota

case class A(override val accidente: Accidente = SinAccidente) extends Nota

case class B(override val accidente: Accidente = SinAccidente) extends Nota


abstract class Figura(val duracion: Int)

case object Redonda extends Figura(1500)

case object Blanca extends Figura(Redonda.duracion / 2)

case object Negra extends Figura(Blanca.duracion / 2)

case object Corchea extends Figura(Negra.duracion / 2)

case object SemiCorchea extends Figura(Corchea.duracion / 2)


case class Tono(octava: Int, nota: Nota)


trait Tocable

case class Sonido(tono: Tono, figura: Figura) extends Tocable

case class Silencio(figura: Figura) extends Tocable

case class Acorde(tonos: List[Tono], figura: Figura) extends Tocable
