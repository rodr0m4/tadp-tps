import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers}
import parser.alphaNum.NotAlphaNumException
import parser.char.ExpectedButFound
import parser.digit.NotADigitException
import parser.letter.NotALetterException
import parser.string.DoesNotStartWithException
import parser.{DoesNotSatisfyPredicateException, EmptyStringException, Parser, alphaNum, anyChar, char, digit, letter, string, void, ~}

import scala.util.{Failure, Success}

class ProjectSpec extends FreeSpec with Matchers with MockFactory {

  import parser.TildeSyntax

  "anyChar should parse hola" in {
    val Success((parsed, remaining)) = anyChar("hola")
    parsed shouldBe 'h'
    remaining shouldBe "ola"
  }

  "anyChar should not parse empty string" in {
    val Failure(reason) = anyChar("")
    reason shouldBe EmptyStringException
  }

  "char('x') should parse string started with x" in {
    val Success((parsed, remaining)) = char('x')("xol")
    parsed shouldBe 'x'
    remaining shouldBe "ol"
  }

  "char('x') should not parse string started with z" in {
    val Failure(reason) = char('x')("zol")
    reason shouldBe ExpectedButFound('x', 'z')
  }

  "char('x') should not parse empty string" in {
    val Failure(reason) = char('x')("")
    reason shouldBe EmptyStringException
  }

  "void should parse hola" in {
    val Success((parsed, remaining)) = void("hola")
    parsed shouldBe()
    remaining shouldBe "ola"
  }

  "void should not parse empty string" in {
    val Failure(reason) = void("")
    reason shouldBe EmptyStringException
  }

  "letter should parse 'karen'" in {
    val Success((parsed, remaining)) = letter("karen")
    parsed shouldBe 'k'
    remaining shouldBe "aren"
  }

  "letter should not parse '0la'" in {
    val Failure(reason) = letter("0la")
    reason shouldBe NotALetterException('0')
  }

  "letter should not parse empty string" in {
    val Failure(reason) = letter("")
    reason shouldBe EmptyStringException
  }

  "digit should parse '1234444'" in {
    val Success((parsed, remaining)) = digit("1234444")
    parsed shouldBe '1'
    remaining shouldBe "234444"
  }

  "digit should not parse 'k1234444'" in {
    val Failure(reason) = digit("k1234444")
    reason shouldBe NotADigitException('k')
  }

  "digit should not parse empty string" in {
    val Failure(reason) = digit("")
    reason shouldBe EmptyStringException
  }

  "alphaNum should parse 'sdfghjk" in {
    val Success((parsed, remaining)) = alphaNum("sdfghjk")
    parsed shouldBe 's'
    remaining shouldBe "dfghjk"
  }

  "alphaNum should parse '1234abcd" in {
    val Success((parsed, remaining)) = alphaNum("1234abcd")
    parsed shouldBe '1'
    remaining shouldBe "234abcd"
  }

  "alphaNum should not parse '#ModoDiablo'" in {
    val Failure(reason) = alphaNum("#ModoDiablo")
    reason shouldBe NotAlphaNumException('#')
  }

  "alphaNum should not parse empty string" in {
    val Failure(reason) = alphaNum("")
    reason shouldBe EmptyStringException
  }

  """string("karen") should parse a string that starts with "karen"""" in {
    val Success((parsed, remaining)) = string("karen")("karen tiene sueño")
    parsed shouldBe "karen"
    remaining shouldBe " tiene sueño"
  }

  """string("karen") should not parse a string that starts with "rodri"""" in {
    val Failure(reason) = string("karen")("rodri tiene sueño")
    reason shouldBe DoesNotStartWithException("karen", "rodri tiene sueño")
  }

  """string("karen") should not parse empty string""" in {
    val Failure(reason) = string("karen")("")
    reason shouldBe EmptyStringException
  }

  "digit <|> letter should parse '5h34'" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("5h34")
    value shouldBe '5'
    remaining shouldBe "h34"
  }

  "digit <|> letter should parse 'lh34'" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("lh34")
    value shouldBe 'l'
    remaining shouldBe "h34"
  }

  "digit <|> letter should not parse '-h34'" in {
    val parser = digit <|> letter
    val Failure(reason) = parser("-h34")
    reason shouldBe NotALetterException('-')
  }

  "char('h') <> digit should parse 'h1'" in {
    val parser = char('h') <> digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe 'h' ~ '1'
    remaining shouldBe ""
  }

  "anyChar <> letter should not parse '123'" in {
    val parser = anyChar <> letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  "digit <> letter should not parse 'abc'" in {
    val parser = digit <> letter
    val Failure(reason) = parser("abc")
    reason shouldBe NotADigitException('a')
  }

  //TODO: Do we care if the second parser is "called"? I think we're fine if the final result is what we expect

  //  "<> when first parse fails it fails and does not call the second parser" in {
  //    val mockedParser = stub[Parser[Any]]
  //    val parser = digit <> mockedParser
  //
  //    (mockedParser.apply _).verify(*).never()
  //
  //    val Failure(reason) = parser("hola")
  //
  //    reason shouldBe NotADigitException('h')
  //  }


  //TODO: Is this really necessary?
  "<> with more complex cases" in {
    def digits: Parser[Int] = digit.+.map(_.mkString.toInt)

    val parser = digits <> anyChar <> digits map {
      case left ~ '+' ~ right => left + right
      case left ~ '*' ~ right => left * right
    }

    val Success((seven, "")) = parser("2+5")
    val Success((four, "")) = parser("2*2")

    seven shouldBe 7
    four shouldBe 4

    val anotherParser: Parser[(String, Int)] = string("val") <> letter.+.map(_.mkString) <> char('=') <> digits map {
      case _ ~ id ~ _ ~ number => (id, number)
    }

    val Success(((id, number), "")) = anotherParser("valx=42") // Yep, no spaces :P

    id shouldBe "x"
    number shouldBe 42
  }

  "char('h') ~> digit should parse 'h1' and return '1'" in {
    val parser = char('h') ~> digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe '1'
    remaining shouldBe ""
  }

  "letter ~> anyChar should not parse '123'" in {
    val parser = letter ~> anyChar
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('1')
  }

  "anyChar ~> letter should not parse '123'" in {
    val parser = anyChar ~> letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  //TODO: Same as similar test case for <>

  //  "~> when first parse fails it fails and does not call the second parser" in {
  //    val mockedParser = stub[Parser[Any]]
  //    val parser = digit ~> mockedParser
  //
  //    (mockedParser.apply _).verify(*).never()
  //
  //    val Failure(reason) = parser("hola")
  //
  //    reason shouldBe NotADigitException('h')
  //  }

  "char('h') <~ digit should parse 'h1' and return 'h'" in {
    val parser = char('h') <~ digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe 'h'
    remaining shouldBe ""
  }

  "letter <~ anyChar should not parse '123'" in {
    val parser = letter <~ anyChar
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('1')
  }

  "anyChar <~ letter should not parse '123'" in {
    val parser = anyChar <~ letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  //TODO: Same as similar tests above
  //TODO: Learn to mock parsers

  //  "<~ when first parse fails it fails and does not call the second parser" in {
  //    val mockedParser = stub[Parser[Any]]
  //    val parser = digit <~ mockedParser
  //
  //    (mockedParser.apply _).verify(*).never()
  //
  //    val Failure(reason) = parser("hola")
  //
  //    reason shouldBe NotADigitException('h')
  //  }

  "satisfies should return success when parsing successfuly and parsed value passes condition" in {
    val parser = digit.satisfies(number => number.toString.toInt < 5)
    val Success((value, remaining)) = parser("1234")
    value shouldBe '1'
    remaining shouldBe "234"
  }

  "satisfies should return faliure when parsing successfuly but parsed value doesn't pass condition" in {
    val parser = digit.satisfies(number => number.toString.toInt > 10)
    val Failure(reason) = parser("1234")
    reason shouldBe DoesNotSatisfyPredicateException('1')
  }

  "satisfies should return faliure when parsing fails" in {
    val parser = digit.satisfies(number => number.toString.toInt > 10)
    val Failure(reason) = parser("abcd")
    reason shouldBe NotADigitException('a')
  }

  "digit ? should parse '2'" in {
    val Success((value, _)) = digit ? "2"
    value shouldBe Some('2')
  }

  "digit ? should not parse 'ff' but should return successfuly without consuming input" in {
    val Success((value, remaining)) = digit ? "ff"
    value shouldBe None
    remaining shouldBe "ff"
  }

  "digit * should parse 'gggg'" in {
    val Success((value, remaining)) = digit * "gggg"
    value shouldBe List()
    remaining shouldBe "gggg"
  }

  "digit * should parse '1234a'" in {
    val Success((value, remaining)) = digit * "1234a"
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "a"
  }

  "digit + should not parse 'f'" in {
    val Failure(reason) = digit + "f"
    reason shouldBe NotADigitException('f')
  }

  "digit + should parse '1234f'" in {
    val Success((value, remaining)) = digit + "1234f"
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "f"
  }

  "digit.sepBy(char('-')) should parse '1'" in {
    val Success((value, remaining)) = digit.sepBy(char('-'))("1")
    value shouldBe List('1')
    remaining shouldBe ""
  }

  "digit.sepBy(char('-')) should parse '1-2-3-4'" in {
    val Success((value, remaining)) = digit.sepBy(char('-'))("1-2-3-4-f")
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "-f"
  }

  "digit.sepBy(char('-')) should not parse 'a-b-c-d'" in {
    val Failure(reason) = digit.sepBy(char('-'))("a-b-c-d")
    reason shouldBe NotADigitException('a')
  }

  "anyChar.const(\"Karen es la mas kpa\") parses hola and replaces parsed 'h'" in {
    val Success((parsed, remaining)) = anyChar.const("Karen es la mas kpa")("hola")
    parsed shouldBe "Karen es la mas kpa"
    remaining shouldBe "ola"
  }

  "char('x').const(true) should not parse \"karen\"" in {
    val Failure(reason) = char('x').const(true)("karen")
    reason shouldBe ExpectedButFound('x', 'k')
  }

  "anyChar.map(_.toUpper) should parse \"batman\" and return 'B'" in {
    val Success((parsed, remaining)) = anyChar.map(_.toUpper)("batman")
    parsed shouldBe 'B'
    remaining shouldBe "atman"
  }

  "digit.map(_.toUpper) should not parse \"batman\"" in {
    val Failure(reason) = digit.map(_.toUpper)("batman")
    reason shouldBe NotADigitException('b')
  }
}
