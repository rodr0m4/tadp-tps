import org.scalatest.{FreeSpec, Matchers}
import parser.alphaNum.NotAlphaNumException
import parser.char.ExpectedButFound
import parser.digit.NotADigitException
import parser.letter.NotALetterException
import parser.string.DoesNotStartWithException
import parser.{DoesNotSatisfyPredicateException, EmptyStringException, Parser, alphaNum, anyChar, char, digit, letter, string, void, ~}

import scala.util.{Failure, Success}

class ProjectSpec extends FreeSpec with Matchers {

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

  "char(specificCharacter) should parse the specified character" in {
    val Success((parsed, remaining)) = char('x')("xol")
    parsed shouldBe 'x'
    remaining shouldBe "ol"
  }

  "char(specificCharacter) should not parse a character that was not specified" in {
    val Failure(reason) = char('x')("zol")
    reason shouldBe ExpectedButFound('x', 'z')
  }

  "char(specificCharacter) should not parse empty string" in {
    val Failure(reason) = char('x')("")
    reason shouldBe EmptyStringException
  }

  "void should parse first character" in {
    val Success((parsed, remaining)) = void("hola")
    parsed shouldBe ()
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

  "alphaNum should not parse a symbol" in {
    val Failure(reason) = alphaNum("#ModoDiablo")
    reason shouldBe NotAlphaNumException('#')
  }

  "alphaNum should not parse empty string" in {
    val Failure(reason) = alphaNum("")
    reason shouldBe EmptyStringException
  }

  "string(specifiedWord) should parse a string that starts with the specified word" in {
    val Success((parsed, remaining)) = string("karen")("karen tiene sueño")
    parsed shouldBe "karen"
    remaining shouldBe " tiene sueño"
  }

  "string(specifiedWord) should not parse a string that does not start with the specified word" in {
    val Failure(reason) = string("karen")("rodri tiene sueño")
    reason shouldBe DoesNotStartWithException("karen", "rodri tiene sueño")
  }

  "string(specifiedWord) should not parse empty string" in {
    val Failure(reason) = string("karen")("")
    reason shouldBe EmptyStringException
  }

  "digit <|> letter should parse a character that is a digit" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("5h34")
    value shouldBe '5'
    remaining shouldBe "h34"
  }

  "digit <|> letter should parse a character that is a letter" in {
    val parser = digit <|> letter
    val Success((value, remaining)) = parser("ah34")
    value shouldBe 'a'
    remaining shouldBe "h34"
  }

  "digit <|> letter should not parse a symbol" in {
    val parser = digit <|> letter
    val Failure(reason) = parser("-h34")
    reason shouldBe NotALetterException('-')
  }

  "parserA <> parserB should succeeded if both parsing finish with success" in {
    val parser = char('h') <> digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe 'h' ~ '1'
    remaining shouldBe ""
  }

  "parserA <> parserB should complete the first parsing but the second one should fail" in {
    val parser = anyChar <> letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  "parserA <> parserB should not complete the second parsing if the first one fails" in {
    val parser = digit <> letter
    val Failure(reason) = parser("abc")
    reason shouldBe NotADigitException('a')
  }

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

  "firstParser ~> secondParser should parse a string and return the value of the second parser" in {
    val parser = char('h') ~> digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe '1'
    remaining shouldBe ""
  }

  "firstParser ~> secondParser should not finish parsing when the first parser fails" in {
    val parser = letter ~> anyChar
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('1')
  }

  "firstParser ~> secondParser should not finish parsing if the second parser fails" in {
    val parser = anyChar ~> letter
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('2')
  }

  "firstParser <~ secondParser should finish parsing and return the value of the first parser" in {
    val parser = char('h') <~ digit
    val Success((parsed, remaining)) = parser("h1")
    parsed shouldBe 'h'
    remaining shouldBe ""
  }

  "firstParser <~ secondParser should not finish parsing if first parser fails" in {
    val parser = letter <~ anyChar
    val Failure(reason) = parser("123")
    reason shouldBe NotALetterException('1')
  }

  "firstParser <~ secondParser should not finish parsing if the second parser fails" in {
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

  "optional parser should successfully parse and return the optional value" in {
    val Success((value, _)) = digit ? "2"
    value shouldBe Some('2')
  }

  "optional parser should not parse value if the original parser fails but opt should return successfully without consuming input" in {
    val Success((value, remaining)) = digit ? "ff"
    value shouldBe None
    remaining shouldBe "ff"
  }

  "parser * should parse word to empty list if the original parser fails" in {
    val Success((value, remaining)) = digit * "gggg"
    value shouldBe List()
    remaining shouldBe "gggg"
  }

  "parser * should parse up to the point when the original parser fails and return parsed values" in {
    val Success((value, remaining)) = digit * "1234a"
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "a"
  }

  "parser + should not parse if the original parser fails" in {
    val Failure(reason) = digit + "f"
    reason shouldBe NotADigitException('f')
  }

  "parser + should parse up to the point when the original parser fails and return parsed values'" in {
    val Success((value, remaining)) = digit + "1234f"
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "f"
  }

  "parserA.sepBy(parserB) should parse word without the separation" in {
    val Success((value, remaining)) = digit.sepBy(char('-'))("1")
    value shouldBe List('1')
    remaining shouldBe ""
  }

  "parserA.sepBy(parserB) should parse up to the point when the parserA fails and separating by the criterion of the parserB" in {
    val Success((value, remaining)) = digit.sepBy(char('-'))("1-2-3-4-f")
    value shouldBe List('1', '2', '3', '4')
    remaining shouldBe "-f"
  }

  "parserA.sepBy(parserB) should not parse if the parserA fails" in {
    val Failure(reason) = digit.sepBy(char('-'))("a-b-c-d")
    reason shouldBe NotADigitException('a')
  }

  "parserA.const(specificValue) should parse and replace the original value for the specificValue" in {
    val Success((parsed, remaining)) = anyChar.const("Karen es la mas kpa")("hola")
    parsed shouldBe "Karen es la mas kpa"
    remaining shouldBe "ola"
  }

  "parserA.const(specificValue) should fail if parserA fails" in {
    val Failure(reason) = char('x').const(true)("karen")
    reason shouldBe ExpectedButFound('x', 'k')
  }

  "parserA.map(transformation) should parse and transform the value" in {
    val Success((parsed, remaining)) = anyChar.map(_.toUpper)("batman")
    parsed shouldBe 'B'
    remaining shouldBe "atman"
  }

  "parserA.map(transformation) should not parse if the parserA fails" in {
    val Failure(reason) = digit.map(_.toUpper)("batman")
    reason shouldBe NotADigitException('b')
  }
}