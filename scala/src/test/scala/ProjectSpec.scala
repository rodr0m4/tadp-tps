import org.scalamock.scalatest.MockFactory
import org.scalatest.{FreeSpec, Matchers, OneInstancePerTest}

class ProjectSpec extends FreeSpec with Matchers with MockFactory {

  "anyChar should parse hola" in {
    val Success(char, remaining) = anyChar("hola")
    char shouldBe 'h'
    remaining shouldBe "ola"
  }

  "anyChar should parse chau" in {
    val Success(char, remaining) = anyChar("chau")
    char shouldBe 'c'
    remaining shouldBe "hau"
  }
  "anyChar should not parse empty string" in {
    val Failure(reason) = anyChar("")
    reason shouldBe "empty string"
  }

  "char('x') should parse string started with x" in {
    val Success(first, remaining) = char('x')("xol")
    first shouldBe 'x'
    remaining shouldBe "ol"
  }
  "char('z') should not parse string started with x" in {
    val Failure(reason) = char('x')("zol")
    reason shouldBe "z is not x"
  }
  "char('z') should not parse empty string" in {
    val Failure(reason) = char('x')("")
    reason shouldBe "empty string"
  }

  "void should parse hola" in {
    val Success(parsed,remaining) = void("hola")
    parsed shouldBe ()
    remaining shouldBe "ola"
  }

  "const replaces whatever this parser parses" in {
    val Success(parsed, _) = anyChar.const("Karen es la mas kpa")("hola")
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
    val Success(parsed, _) = parser(input)
    parsed shouldBe 'B'
  }

  "digit should parse when is digit" in {
    val Success(parsed, _) = digit("1234444")
    parsed shouldBe '1'
  }

  "digit should not parse when is non digit" in {
    val Failure(reason) = digit("k1234444")
    reason shouldBe "k is not digit"
  }

  "letter should parse when is a letter" in {
    val Success(parsed, _) = letter("karen")
    parsed shouldBe 'k'
  }

  "letter should not parse when is non letter" in {
    val Failure(reason) = letter("0la")
    reason shouldBe "0 is not letter"
  }

  "alphaNum should parse when is digit or num" in {
    val Success(parsed, _) = alphaNum("sdfghjk")
    parsed shouldBe 's'
  }

  "alphaNum should not parse when is non digit or num" in {
    val Failure(reason) = alphaNum("#ModoDiablo")
    reason shouldBe "# is neither letter nor digit"
  }

  "<> when both parses parse we get a new one" in {
    val parser = char('h') <> digit
    val Success(parsed, remaining) = parser("h1")

    parsed shouldBe ('h', '1')
    remaining shouldBe ""
  }

  "<> when second parse fails it fails" in {
    val parser = anyChar <> letter
    val Failure(reason) = parser("123")

    reason shouldBe "2 is not letter"
  }

  "<> when first parse fails it fails and does not call the second parser" in {
    val mockedParser = stub[Parser[Any]]
    val parser = digit <> mockedParser

    (mockedParser.apply _).verify(*).never()

    val Failure(reason) = parser("hola")

    reason shouldBe "h is not digit"
  }

  """string("karen") should parse when input is a string that starts with "karen"""" in {
    val Success(parsed, remaining) = string("karen")("karen tiene sueño")
    parsed shouldBe "karen"
    remaining shouldBe " tiene sueño"
  }
  """string("karen") should not parse when input is not a string that starts with "karen"""" in {
    val Failure(reason) = string("karen")("rodri tiene sueño")
    reason shouldBe "rodri tiene sueño does not start with karen"
  }

  "~> when both parses parse we get a new one" in {
    val parser = char('h') ~> digit
    val Success(parsed, remaining) = parser("h1")

    parsed shouldBe '1'
    remaining shouldBe ""
  }

  "~> when second parse fails it fails" in {
    val parser = anyChar ~> letter
    val Failure(reason) = parser("123")

    reason shouldBe "2 is not letter"
  }

  "~> when first parse fails it fails and does not call the second parser" in {
    val mockedParser = stub[Parser[Any]]
    val parser = digit ~> mockedParser

    (mockedParser.apply _).verify(*).never()

    val Failure(reason) = parser("hola")

    reason shouldBe "h is not digit"
  }

  "<~ when both parses parse we get a new one" in {
    val parser = char('h') <~ digit
    val Success(parsed, remaining) = parser("h1")

    parsed shouldBe 'h'
    remaining shouldBe ""
  }

  "<~ when second parse fails it fails" in {
    val parser = anyChar <~ letter
    val Failure(reason) = parser("123")

    reason shouldBe "2 is not letter"
  }

  "<~ when first parse fails it fails and does not call the second parser" in {
    val mockedParser = stub[Parser[Any]]
    val parser = digit <~ mockedParser

    (mockedParser.apply _).verify(*).never()

    val Failure(reason) = parser("hola")

    reason shouldBe "h is not digit"
  }
}
