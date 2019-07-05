package musiquita

import musiquita.Musiquita.Octava
import parser._

object Musiquita {
  type Octava = Int

  lazy val tono: Parser[Tono] = numero <> nota map Tono
  lazy val sonido: Parser[Sonido] = tono <> figura map Sonido
  lazy val acordeExplicito: Parser[Acorde] = tono.sepBy('-') <> figura map Acorde
  lazy val acordeTonal: Parser[Acorde] = sonido <> mayorOMenor map {
    case Sonido(Tono(octava, nota), figura) ~ mayorOMenor =>
      mayorOMenor(nota)(octava, figura)
  }

  lazy val mayorOMenor: Parser[Nota => (Octava, Figura) => Acorde] = oneOf(
    'm' -> { nota: Nota => nota.m _ },
    'M' -> { nota: Nota => nota.M _ }
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
  ) <> oneOf( '#' -> ♯, 'b' -> ♭ ).?.map(_.getOrElse(SinAccidente)) map {
    // Deberíamos fallar en estos casos?
    case B ~ `♯` => C()
    case C ~ `♭` => B()
    case A ~ `♯` => F()
    case F ~ `♭` => A()

    case nota ~ accidente => nota(accidente)
  }

  private val numero : Parser[Int] = digit.map(_.toString.toInt)
}

sealed trait Accidente
case object SinAccidente extends Accidente
case object ♯ extends Accidente
case object ♭ extends Accidente

sealed trait Nota {
  val accidente: Accidente

  lazy val `♯`: Nota = Nota.idToNota(this.toId + 1)
  lazy val `♭`: Nota = Nota.idToNota(this.toId - 1)

  def m(octava: Octava = 0, figura: Figura): Acorde =
    Acorde((this :: this + 3 :: this + 7 :: Nil).map(Tono(octava, _)), figura)

  def M(octava: Octava = 0, figura: Figura): Acorde =
    Acorde((this :: this + 4 :: this + 7 :: Nil).map(Tono(octava, _)), figura)

  def +(cantidadDeSemitonos: Int): Nota = 1.to(cantidadDeSemitonos).foldLeft(this) {
    case (nota, _) => nota.`♯`
  }

  private def toId: Int = Nota.notaToId(this)
}

object Nota {
  private val notaToIdTable: Map[Nota, Int] = Map(
    C(♭) -> 11, // TODO: Sacar?
    C() -> 0,
    C(♯) -> 1,
    D(♭) -> 1,
    D() -> 2,
    D(♯) -> 3,
    E(♭) -> 3,
    E() -> 4,
    F(♭) -> 4,  // TODO: Sacar?
    E(♯) -> 5,  // TODO: Sacar?
    F() -> 5,
    F(♯) -> 6,
    G(♭) -> 6,
    G() -> 7,
    G(♯) -> 8,
    A(♭) -> 8,
    A() -> 9,
    A(♯) -> 10,
    B() -> 11,
    B(♯) -> 0 // TODO: Sacar?
  )

  private val idToNotaTable: Map[Int, Nota] = Map(
    0 -> C(),
    1 -> C(♯),
    2 -> D(),
    3 -> D(♯),
    4 -> E(),
    5 -> F(),
    6 -> F(♯),
    7 -> G(),
    8 -> G(♯),
    9 -> A(),
    10 -> A(♯),
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



