import org.scalatest.{FreeSpec, Matchers}
import parser.{DoesNotSatisfyPredicateException, EmptyStringException, ExpectedButFound, NotADigitException, NotALetterException, NotAlphaNumException, OrException, alphaNum, anyChar, char, digit, id, letter, string, void}

import scala.util.{Failure, Success}

class ParserSpec extends FreeSpec with Matchers {

  import parser.TildeSyntax

  "anyChar should parse any character" in {
    val Success((parsed, remaining)) = anyChar("hola")
    parsed shouldBe 'h'
    remaining shouldBe "ola"
  }

  "anyChar should not parse empty string" in {
    val Failure(reason) = anyChar("")
    reason shouldBe EmptyStringException
  }

  "char should parse the specified character" in {
    val Success((parsed, remaining)) = char('x')("xol")
    parsed shouldBe 'x'
    remaining shouldBe "ol"
  }

  "char should not parse a character that was not specified" in {
    val Failure(reason) = char('x')("zol")
    reason shouldBe ExpectedButFound('x', 'z')
  }

  "char should not parse empty string" in {
    val Failure(reason) = char('x')("")
    reason shouldBe EmptyStringException
  }

  "void should parse first character" in {
    val Success((parsed, remaining)) = void("hola")
    parsed shouldBe()
    remaining shouldBe "ola"
  }

  "void should not parse empty string" in {
    val Failure(reason) = void("")
    reason shouldBe EmptyStringException
  }

  "letter should parse any letter" in {
    val Success((parsed, remaining)) = letter("karen")
    parsed shouldBe 'k'
    remaining shouldBe "aren"
  }

  "letter should not parse a character that is not a letter" in {
    val Failure(reason) = letter("0la")
    reason shouldBe NotALetterException('0')
  }

  "letter should not parse empty string" in {
    val Failure(reason) = letter("")
    reason shouldBe EmptyStringException
  }

  "digit should parse any digit" in {
    val Success((parsed, remaining)) = digit("1234444")
    parsed shouldBe '1'
    remaining shouldBe "234444"
  }

  "digit should not parse a character that is not a digit" in {
    val Failure(reason) = digit("k1234444")
    reason shouldBe NotADigitException('k')
  }

  "digit should not parse empty string" in {
    val Failure(reason) = digit("")
    reason shouldBe EmptyStringException
  }

  "alphaNum should parse any letter" in {
    val Success((parsed, remaining)) = alphaNum("sdfghjk")
    parsed shouldBe 's'
    remaining shouldBe "dfghjk"
  }

  "alphaNum should parse any digit" in {
    val Success((parsed, remaining)) = alphaNum("1234abcd")
    parsed shouldBe '1'
    remaining shouldBe "234abcd"
  }

  "alphaNum should not parse a character that is neither a letter nor a digit" in {
    val Failure(reason) = alphaNum("#ModoDiablo")
    reason shouldBe NotAlphaNumException('#')
  }

  "alphaNum should not parse empty string" in {
    val Failure(reason) = alphaNum("")
    reason shouldBe EmptyStringException
  }

  "string should parse an input that starts with the specified string" in {
    val Success((parsed, remaining)) = string("karen")("karen tiene sueño")
    parsed shouldBe "karen"
    remaining shouldBe " tiene sueño"
  }

  "string should not parse an input that does not start with the specified string" in {
    val Failure(reason) = string("karen")("rodri tiene sueño")
    reason shouldBe ExpectedButFound('k', 'r')
  }

  "string should not parse empty string" in {
    val Failure(reason) = string("karen")("")
    reason shouldBe EmptyStringException
  }

  "<|> should parse something that's parsed by its first parser" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("5h34")
    value shouldBe '5'
    remaining shouldBe "h34"
  }

  "<|> should parse something that's parsed by its second parser" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("ah34")
    value shouldBe 'a'
    remaining shouldBe "h34"
  }

  "<|> should not parse something that's not parsed by any of its parsers" in {
    val parser = digit <|> letter
    val Failure(reason) = parser("-h34")
    reason shouldBe OrException(NotADigitException('-'), NotALetterException('-'))
  }

  "<> should parse a sequence of something parsed by the first parser followed by something parsed by the second" in {
    val parser = char('h') <> digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe 'h' ~ '1'
    remaining shouldBe ""
  }

  "<> should not parse something parsed by the first parser followed by something not parsed by the second" in {
    val parser = anyChar <> letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  "<> should not parse something not parsed by the first parser" in {
    val parser = digit <> letter
    val Failure(reason) = parser("abc")
    reason shouldBe NotADigitException('a')
  }

  "~> should parse something parsed by the first parser followed by something parsed by the second and return the value of the second" in {
    val parser = char('h') ~> digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe '1'
    remaining shouldBe ""
  }

  "~> should not parse something that's not parsed by the first parser" in {
    val parser = letter ~> anyChar
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('1')
  }

  "~> should not parse something that's parsed by the first parser followed by something that's not parsed by the second" in {
    val parser = anyChar ~> letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  "<~ should parse something parsed by the first parser followed by something parsed by the second and return the value of the first" in {
    val parser = char('h') <~ digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe 'h'
    remaining shouldBe ""
  }

  "<~ should not parse something that's not parsed by the first parser" in {
    val parser = letter <~ anyChar
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('1')
  }

  "<~ should not parse something that's parsed by the first parser followed by something that's not parsed by the second" in {
    val parser = anyChar <~ letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  "satisfies should return success when parsing successfully and parsed value passes condition" in {
    val parser = digit.satisfies(number => number.toString.toInt < 5)
    val Success((value, remaining)) = parser("1234")
    value shouldBe '1'
    remaining shouldBe "234"
  }

  "satisfies should return failure when parsing successfully but parsed value doesn't pass condition" in {
    val parser = digit.satisfies(number => number.toString.toInt > 10)
    val Failure(reason) = parser("1234")
    reason shouldBe DoesNotSatisfyPredicateException('1')
  }

  "satisfies should return failure when parsing fails" in {
    val parser = digit.satisfies(number => number.toString.toInt > 10)
    val Failure(reason) = parser("abcd")
    reason shouldBe NotADigitException('a')
  }

  "opt should parse successfully something parsed by the original parser" in {
    val Success((value, remaining)) = digit ? "2"
    value shouldBe Some('2')
    remaining shouldBe ""
  }

  "opt should return successfully and not consume any input when the original parser fails" in {
    val Success((value, remaining)) = digit ? "ff"
    value shouldBe None
    remaining shouldBe "ff"
  }

  "* should parse to empty list if the original parser fails" in {
    val Success((value, remaining)) = digit * "gggg"
    value shouldBe List()
    remaining shouldBe "gggg"
  }

  "* should parse up to the point when the original parser fails and return a list of parsed values" in {
    val Success((value, remaining)) = digit * "1234a"
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "a"
  }

  "+ should not parse if the original parser fails" in {
    val Failure(reason) = digit + "f"
    reason shouldBe NotADigitException('f')
  }

  "+ should parse up to the point when the original parser fails and return a list of parsed values'" in {
    val Success((value, remaining)) = digit + "1234f"
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "f"
  }

  "sepBy should parse something parsed by the original parser" in {
    val Success((value, remaining)) = digit.sepBy(char('-'))("1")
    value shouldBe List('1')
    remaining shouldBe ""
  }

  "sepBy should parse a sequence parsed by the original parser until it fails separated by something parsed by its argument and return a list of parsed values" in {
    val Success((value, remaining)) = digit.sepBy(char('-'))("1-2-3-4-f")
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "-f"
  }

  "sepBy should fail if the original parser fails" in {
    val Failure(reason) = digit.sepBy(char('-'))("a-b-c-d")
    reason shouldBe NotADigitException('a')
  }

  "const should parse successfuly when the original parser does and replace the parsed value for its argument" in {
    val Success((parsed, remaining)) = anyChar.const("Karen es la mas kpa")("hola")
    parsed shouldBe "Karen es la mas kpa"
    remaining shouldBe "ola"
  }

  "const should fail if the original parser fails" in {
    val Failure(reason) = char('x').const(true)("karen")
    reason shouldBe ExpectedButFound('x', 'k')
  }

  "map should transform the value on successful parsing" in {
    val Success((parsed, remaining)) = anyChar.map(_.toUpper)("batman")
    parsed shouldBe 'B'
    remaining shouldBe "atman"
  }

  "map should fail if the original parser fails" in {
    val Failure(reason) = digit.map(_.toUpper)("batman")
    reason shouldBe NotADigitException('b')
  }

  "id should not consume any character" in {
    val Success((parsed, remaining)) = id("abcdefg")
    parsed shouldBe()
    remaining shouldBe "abcdefg"
  }

  "oneOf should parse something parsed by one of its arguments" in {
    val Success((parsed, remaining)) = parser.oneOf('a', 'b', 'c', 'd')("cdefg")
    parsed shouldBe 'c'
    remaining shouldBe "defg"
  }

  "oneOf should not parse something not parsed by any of its arguments" in {
    val Failure(reason) = parser.oneOf(letter, digit, alphaNum)("-+*/")
    reason shouldBe OrException(List(NotALetterException('-'), NotADigitException('-'), NotAlphaNumException('-')))
  }

  "oneOf should not parse empty string" in {
    val Failure(reason) = parser.oneOf('a', "bc")("")
    reason shouldBe EmptyStringException
  }

  "mapError should map error with provided function" in {
    case class CustomException(e: Throwable) extends Exception
    val Failure(reason) = digit.mapError { case e => CustomException(e) }("a")
    reason shouldBe CustomException(NotADigitException('a'))
  }

  "mapError should not change anything on successful parse" in {
    case class CustomException(e: Throwable) extends Exception
    val Success((parsed, remaining)) = digit.mapError { case e => CustomException(e) }("123")
    parsed shouldBe '1'
    remaining shouldBe "23"
  }
}
