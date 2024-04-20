package tethys

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.commons.{Token, TokenNode}
import tethys.commons.TokenNode.obj
import tethys.readers.tokens.QueueIterator
import tethys.writers.tokens.SimpleTokenWriter.SimpleTokenWriterOps

class DerivationSpec extends AnyFlatSpec with Matchers {
  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  it should "compile and correctly write and read product" in {
    case class Person(id: Int, name: String) derives JsonWriter, JsonReader

    case class Wrapper(person: Person) derives JsonWriter, JsonReader

    Person(2, "Peter").asTokenList shouldBe obj(
      "id" -> 2,
      "name" -> "Peter"
    )

    Wrapper(Person(3, "Parker")).asTokenList shouldBe obj(
      "person" -> obj("id" -> 3, "name" -> "Parker")
    )

    read[Person](obj("id" -> 1, "name" -> "abc")) shouldBe Person(1, "abc")
    read[Person](
      obj(
        "abd" -> 3,
        "name" -> "abc",
        "id" -> 1)
    ) shouldBe Person(1, "abc")

    read[Wrapper](
      obj(
        "abc" -> 5,
        "person" -> obj("id" -> 3, "name" -> "Parker")
      )
    ) shouldBe Wrapper(Person(3, "Parker"))
  }

  it should "compile and correctly write sum" in {
    sealed trait A derives JsonWriter

    case class B(b: Int, i: String) extends A derives JsonWriter

    case class C(c: String) extends A derives JsonWriter


    (B(2, "abc"): A).asTokenList shouldBe obj(
      "b" -> 2,
      "i" -> "abc"
    )
  }


}
