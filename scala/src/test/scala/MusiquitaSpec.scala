import musiquita.{A, Blanca, C, Corchea, E, G, Musiquita, Negra, Silencio, SinAccidente, Sonido, Sostenido, Tono}
import org.scalatest.{FreeSpec, Matchers}
import parser.{DoesNotStartWithException, ExpectedButFound}
import musiquita.Musiquita._

import scala.util.{Failure, Success}

class MusiquitaSpec extends FreeSpec with Matchers {
  "silencio should parse a silence" in {
    val Success((parsed, remaining)) = silencio("- A")
    parsed shouldBe Silencio(Negra)
    remaining shouldBe " A"
  }

  "silencio should not parse a figure" in {
    val Failure(reason) = silencio("4 - A")
    reason shouldBe ExpectedButFound('~', '4')
  }

  "nota should parse singleton note" in {
    val Success((parsed, remaining)) = nota("A - A")
    parsed shouldBe A()
    remaining shouldBe " - A"
  }

  "nota should parse composed note" in {
    val Success((parsed, remaining)) = nota("A#- A")
    parsed shouldBe A(Sostenido)
    remaining shouldBe "- A"
  }

  "nota should not parse a silence" in {
    val Failure(reason) = nota("- A")
    reason shouldBe ExpectedButFound('B', '-')
  }

  "figura should parse a figure" in {
    val Success((parsed, remaining)) = figura("1/2A")
    parsed shouldBe Blanca
    remaining shouldBe "A"
  }

  "figura should not parse something that is not a figure" in {
    val Failure(reason) = figura("1//2")
    reason shouldBe DoesNotStartWithException("16", "/2")
  }

  "tono should parse a tone" in {
    val Success((parsed, remaining)) = tono("6A#1/4")
    parsed shouldBe A(Sostenido)
    remaining shouldBe "1/4"
  }

  "tono should not parse a figure" in {
    val Failure(reason) = Musiquita.tono("1/46A#")
    reason shouldBe ExpectedButFound('B', '/')
  }

  "sonido should parse a sound" in {
    val Success((parsed, remaining)) = sonido("6A#1/4")
    parsed shouldBe Sonido(Tono(6, A(Sostenido)), Negra)
    remaining shouldBe ""
  }

  "sonido should not parse a figure followed by a tone" in {
    val Failure(reason) = Musiquita.sonido("1/46A#")
    reason shouldBe ExpectedButFound('B', '/')
  }

  "acorde should parse an explicit chord" in {
    val Success((parsed, remaining)) = Musiquita.acorde("6A+6C#+2G1/8")
    val List(firstTone, secondTone, thirdTone) = parsed.tonos

    firstTone shouldBe Tono(6, A(SinAccidente))
    secondTone shouldBe Tono(6, C(Sostenido))
    thirdTone shouldBe Tono(2, G(SinAccidente))
    parsed.figura shouldBe Corchea
    remaining shouldBe ""
  }

  "acorde should not parse a figure" in {
    val Failure(reason) = Musiquita.acorde("1/2")
    reason shouldBe ExpectedButFound('B', '/')
  }

  "acorde should parse mayor chord" in {
    val Success((parsed, remaining)) = Musiquita.acorde("6AM1/2")
    val List(firstTone, secondTone, thirdTone) = parsed.tonos
    firstTone shouldBe Tono(6, A(SinAccidente))
    secondTone shouldBe Tono(6, C(Sostenido))
    thirdTone shouldBe Tono(6, E())
    parsed.figura shouldBe Blanca
    remaining shouldBe ""
  }

  "acorde should parse minor chord" in {
    val Success((parsed, remaining)) = Musiquita.acorde("6Am1/2")
    val List(firstTone, secondTone, thirdTone) = parsed.tonos
    firstTone shouldBe Tono(6, A())
    secondTone shouldBe Tono(6, C())
    thirdTone shouldBe Tono(6, E())
    parsed.figura shouldBe Blanca
    remaining shouldBe ""
  }

  "acorde should not parse a silence" in {
    val Failure(reason) = Musiquita.acorde("-")
    reason shouldBe ExpectedButFound('B', '/')
  }
}