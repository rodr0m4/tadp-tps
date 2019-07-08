import musiquita.{A, Blanca, C, Corchea, E, F, G, Musiquita, Negra, Nota, Redonda, Silencio, SinAccidente, Sostenido}
import org.scalatest.{FreeSpec, Matchers}
import parser.{DoesNotStartWithException, ExpectedButFound}

import scala.util.{Failure, Success}

class MusiquitaSpec extends FreeSpec with Matchers{
  "silencio should parse a silence" in {
    val Success((parsed, remaining)) = Musiquita.silencio("- A")
    val Silencio(nota) = parsed
    nota.duracion shouldBe Redonda.duracion/4
    remaining shouldBe " A"
  }
  "silencio should not parse a figure" in {
    val Failure(reason) = Musiquita.silencio("4 - A")
    reason shouldBe ExpectedButFound('~', '4')
  }
  "nota should parse singleton note" in {
    val Success((parsed, remaining)) = Musiquita.nota("A - A")
    parsed shouldBe A()
    remaining shouldBe " - A"
  }
  "nota should parse composed note" in {
    val Success((parsed, remaining)) = Musiquita.nota("A#- A")
    parsed shouldBe A(Sostenido)
    remaining shouldBe "- A"
  }
  "nota should not parse a silence" in {
    val Failure(reason) = Musiquita.nota("- A")
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
    parsed.nota shouldBe A(Sostenido)
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
    tono.nota shouldBe A(Sostenido)
    parsed.figura shouldBe Negra
    remaining shouldBe ""
  }

  //TODO
  "sonido should not parse a figure followed by a tone" in {
//    val Failure(reason) = Musiquita.sonido("1/46A#")
//    reason shouldBe ExpectedButFound("", "")
  }

  "acorde should parse an explicit chord" in {
      val Success((parsed, remaining)) = Musiquita.acorde("6A+6C#+2G1/8")
      val List(firstTone, secondTone, thirdTone) = parsed.tonos
      firstTone.octava shouldBe 6
      firstTone.nota shouldBe A(SinAccidente)
      secondTone.octava shouldBe 6
      secondTone.nota shouldBe C(Sostenido)
      thirdTone.octava shouldBe 2
      thirdTone.nota shouldBe G(SinAccidente)
      parsed.figura shouldBe Corchea
      remaining shouldBe ""
  }

  //TODO:
  "acorde should not parse a figure" in {
//      val Failure(reason) = Musiquita.acorde("1/2")
//      reason shouldBe ExpectedButFound("","")
  }

  "acorde should parse mayor chord" in {
      val Success((parsed, remaining)) = Musiquita.acorde("6AM1/2")
      val List(firstTone, secondTone, thirdTone) = parsed.tonos
      firstTone.octava shouldBe 6
      firstTone.nota shouldBe A(SinAccidente)
      secondTone.octava shouldBe 6
      secondTone.nota shouldBe C(Sostenido)
      thirdTone.octava shouldBe 6
      thirdTone.nota shouldBe E(SinAccidente)
      parsed.figura shouldBe Blanca
      remaining shouldBe ""
  }

  "acorde should parse minor chord" in {
    val Success((parsed, remaining)) = Musiquita.acorde("6Am1/2")
    val List(firstTone, secondTone, thirdTone) = parsed.tonos
    firstTone.octava shouldBe 6
    firstTone.nota shouldBe A(SinAccidente)
    secondTone.octava shouldBe 6
    secondTone.nota shouldBe C(SinAccidente)
    thirdTone.octava shouldBe 6
    thirdTone.nota shouldBe E(SinAccidente)
    parsed.figura shouldBe Blanca
    remaining shouldBe ""
  }

  //TODO:
  "acorde should not parse a silence" in {
//    val Failure(reason) = Musiquita.acorde("-")
//    reason shouldBe ExpectedButFound("","")
  }
}
