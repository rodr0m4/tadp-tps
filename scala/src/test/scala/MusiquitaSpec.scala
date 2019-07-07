import musiquita.{Blanca, Musiquita, Negra, Nota, Redonda, Silencio, SinAccidente, F}
import org.scalatest.{FreeSpec, Matchers}
import parser.char.ExpectedButFound
import parser.string.DoesNotStartWithException

import scala.util.{Failure, Success}

class MusiquitaSpec extends FreeSpec with Matchers{
  val idOfA = 9
  "silencio should parse a silence" in {
    val Success((parsed, remaining)) = Musiquita.silencio("-A")
    val Silencio(nota) = parsed
    nota.duracion shouldBe Redonda.duracion/4
    remaining shouldBe "A"
  }
  "silencio should not parse a figure" in {
    val Failure(reason) = Musiquita.silencio("4-A")
    reason shouldBe ExpectedButFound('~', '4')
  }
  "nota should parse singleton note" in {
    val Success((parsed, remaining)) = Musiquita.nota("A-A")
    Nota.notaToId(parsed) shouldBe idOfA
    remaining shouldBe "-A"
  }
  "nota should parse composed note" in {
    val Success((parsed, remaining)) = Musiquita.nota("A#-A")
    Nota.notaToId(parsed) shouldBe idOfA + 1
    remaining shouldBe "-A"
  }
  "nota should not parse a silence" in {
    val Failure(reason) = Musiquita.nota("-A")
    reason shouldBe ExpectedButFound('B','-')
  }
  "figura should parse a figure" in {
    val Success((parsed, remaining)) = Musiquita.figura("1/2A")
    parsed shouldBe Blanca
    remaining shouldBe "A"
  }
  "figura should not parse something that is not a figure" in {
    val Failure(reason) = Musiquita.figura("1//2")
    reason shouldBe DoesNotStartWithException("16","/2")
  }
  "tono should parse a tone" in {
    val Success((parsed, remaining)) = Musiquita.tono("6A#1/4")
    parsed.octava shouldBe 6
    parsed.nota shouldBe F(SinAccidente)
    remaining shouldBe "1/4"
  }

  //TODO PLIS ¿Qué espera esta excepcion ExpectedButFound?
  "tono should not parse a figure" in {
//    val Failure(reason) = Musiquita.tono("1/46A#")
//    reason shouldBe ExpectedButFound('','1')
  }

  "sonido should parse a sound" in {
    val Success((parsed, remaining)) = Musiquita.sonido("6A#1/4")
    val tono = parsed.tono
    tono.octava shouldBe 6
    tono.nota shouldBe F(SinAccidente)
    parsed.figura shouldBe Negra
    remaining shouldBe ""
  }

  //TODO
  "sonido should not parse a figure followed by a tone" in {
//    val Failure(reason) = Musiquita.sonido("1/46A#")
//    reason shouldBe ExpectedButFound("", "")
  }
}
