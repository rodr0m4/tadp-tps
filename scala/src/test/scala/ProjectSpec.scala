import org.scalatest.{FreeSpec, Matchers}

class ProjectSpec extends FreeSpec with Matchers {

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
}
