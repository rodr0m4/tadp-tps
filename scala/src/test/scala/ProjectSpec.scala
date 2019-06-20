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
}
