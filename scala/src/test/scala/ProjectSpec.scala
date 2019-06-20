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
}
