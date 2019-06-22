import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}
import parser.alphaNum.NotAlphaNumException
import parser.char.ExpectedButFound
import parser.charSatisfies.EmptyStringException
import parser.digit.NotADigitException
import parser.letter.NotALetterException
import parser.string.DoesNotStartWithException
import parser.{DoesNotSatisfyPredicateException, Parser, alphaNum, anyChar, char, digit, letter, string, void}

import scala.util.{Failure, Success}

class ProjectSpec extends FreeSpec with Matchers with MockFactory {

  "anyChar should parse hola" in {
    val Success((char, remaining)) = anyChar("hola")
    char shouldBe 'h'
    remaining shouldBe "ola"
  }

  "anyChar should parse chau" in {
    val Success((char, remaining)) = anyChar("chau")
    char shouldBe 'c'
    remaining shouldBe "hau"
  }
  "anyChar should not parse empty string" in {
    val Failure(reason) = anyChar("")
    reason shouldBe EmptyStringException
  }

  "char('x') should parse string started with x" in {
    val Success((first, remaining)) = char('x')("xol")
    first shouldBe 'x'
    remaining shouldBe "ol"
  }
  "char('z') should not parse string started with x" in {
    val Failure(reason) = char('x')("zol")
    reason shouldBe ExpectedButFound('x', 'z')
  }
  "char('z') should not parse empty string" in {
    val Failure(reason) = char('x')("")
    reason shouldBe EmptyStringException
  }

  "void should parse hola" in {
    val Success((parsed,remaining)) = void("hola")
    parsed shouldBe ()
    remaining shouldBe "ola"
  }

  "const replaces whatever this parser parses" in {
    val Success((parsed, _)) = anyChar.const("Karen es la mas kpa")("hola")
    parsed shouldBe "Karen es la mas kpa"
  }

  "parser.const fails when parser fails" in {
    val parser = char('x')
    val input = "karen"
    parser.const()(input) shouldBe parser(input)
  }

  "parser.map transform to upper" in {
    val parser = anyChar.map(_.toUpper)
    val input = "batman"
    val Success((parsed, _)) = parser(input)
    parsed shouldBe 'B'
  }

  "digit should parse when is digit" in {
    val Success((parsed, _)) = digit("1234444")
    parsed shouldBe '1'
  }

  "digit should not parse when is non digit" in {
    val Failure(reason) = digit("k1234444")
    reason shouldBe NotADigitException('k')
  }

  "letter should parse when is a letter" in {
    val Success((parsed, _)) = letter("karen")
    parsed shouldBe 'k'
  }

  "letter should not parse when is non letter" in {
    val Failure(reason) = letter("0la")
    reason shouldBe NotALetterException('0')
  }

  "alphaNum should parse when is digit or num" in {
    val Success((parsed, _)) = alphaNum("sdfghjk")
    parsed shouldBe 's'
  }

  "alphaNum should not parse when is non digit or num" in {
    val Failure(reason) = alphaNum("#ModoDiablo")
    reason shouldBe NotAlphaNumException('#')
  }

  "<> when both parses parse we get a new one" in {
    val parser = char('h') <> digit
    val Success((parsed, remaining)) = parser("h1")

    parsed shouldBe ('h', '1')
    remaining shouldBe ""
  }

  "<> when second parse fails it fails" in {
    val parser = anyChar <> letter
    val Failure(reason) = parser("123")

    reason shouldBe NotALetterException('2')
  }

  "<> when first parse fails it fails and does not call the second parser" in {
    val mockedParser = stub[Parser[Any]]
    val parser = digit <> mockedParser

    (mockedParser.apply _).verify(*).never()

    val Failure(reason) = parser("hola")

    reason shouldBe NotADigitException('h')
  }

  """string("karen") should parse when input is a string that starts with "karen"""" in {
    val Success((parsed, remaining)) = string("karen")("karen tiene sueño")
    parsed shouldBe "karen"
    remaining shouldBe " tiene sueño"
  }

  """string("karen") should not parse when input is not a string that starts with "karen"""" in {
    val Failure(reason) = string("karen")("rodri tiene sueño")
    reason shouldBe DoesNotStartWithException("karen", "rodri tiene sueño")
  }

  "~> when both parses parse we get a new one" in {
    val parser = char('h') ~> digit
    val Success((parsed, remaining)) = parser("h1")

    parsed shouldBe '1'
    remaining shouldBe ""
  }

  "~> when second parse fails it fails" in {
    val parser = anyChar ~> letter
    val Failure(reason) = parser("123")

    reason shouldBe NotALetterException('2')
  }

  "~> when first parse fails it fails and does not call the second parser" in {
    val mockedParser = stub[Parser[Any]]
    val parser = digit ~> mockedParser

    (mockedParser.apply _).verify(*).never()

    val Failure(reason) = parser("hola")

    reason shouldBe NotADigitException('h')
  }

  "<~ when both parses parse we get a new one" in {
    val parser = char('h') <~ digit
    val Success((parsed, remaining)) = parser("h1")

    parsed shouldBe 'h'
    remaining shouldBe ""
  }

  "<~ when second parse fails it fails" in {
    val parser = anyChar <~ letter
    val Failure(reason) = parser("123")

    reason shouldBe NotALetterException('2')
  }

  "<~ when first parse fails it fails and does not call the second parser" in {
    val mockedParser = stub[Parser[Any]]
    val parser = digit <~ mockedParser

    (mockedParser.apply _).verify(*).never()

    val Failure(reason) = parser("hola")

    reason shouldBe NotADigitException('h')
  }

  //to do: How to mock parsers

  "<|> when the first parser fails and the second one parses we get the second value" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("lh34")

    value shouldBe 'l'
    remaining shouldBe "h34"
  }

  "satisfies when the parser parses and the value passes the condition we get a success" in {
    val parser = digit.map(_.toString.toInt).satisfies(number => number < 5)
    val Success((value, remaining)) = parser("1234")

    value shouldBe 1
    remaining shouldBe "234"
  }

  "satisfies when the parser parses but the value does not pass the conditino we get a failure" in {
    val parser = digit.map(_.toString.toInt).satisfies(number => number > 10)
    val Failure(reason) = parser("1234")

    reason shouldBe DoesNotSatisfyPredicateException(1)
  }
}
