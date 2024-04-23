package tethys

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode.obj
import tethys.commons.{Token, TokenNode}
import tethys.readers.tokens.QueueIterator
import tethys.writers.tokens.SimpleTokenWriter.SimpleTokenWriterOps
import tethys.derivation.Defaults

class DerivationSpec extends AnyFlatSpec with Matchers {
  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  it should "compile and correctly write and read product" in {
    case class Person(id: Int, name: String, phone: Option[String], default: String = "") derives JsonWriter, JsonReader

    case class Wrapper(person: Person) derives JsonWriter, JsonReader

    Person(2, "Peter", None).asTokenList shouldBe obj(
      "id" -> 2,
      "name" -> "Peter",
      "default" -> ""
    )

    Wrapper(Person(3, "Parker", None, "abc")).asTokenList shouldBe obj(
      "person" -> obj("id" -> 3, "name" -> "Parker", "default" -> "abc")
    )

    read[Person](obj("id" -> 1, "name" -> "abc")) shouldBe Person(1, "abc", None)

    read[Person](
      obj(
        "abd" -> 3,
        "name" -> "abc",
        "id" -> 1,
        "default" -> "abc"
      )
    ) shouldBe Person(1, "abc", None, "abc")

    read[Wrapper](
      obj(
        "abc" -> 5,
        "person" -> obj("id" -> 3, "name" -> "Parker", "phone" -> "123")
      )
    ) shouldBe Wrapper(Person(3, "Parker", Some("123")))
  }

  it should "compile and correctly write sum" in {
    sealed trait A derives JsonObjectWriter

    case class B(b: Int, i: String) extends A derives JsonObjectWriter

    case class C(c: String) extends A derives JsonObjectWriter


    (B(2, "abc"): A).asTokenList shouldBe obj(
      "b" -> 2,
      "i" -> "abc"
    )
  }

  it should "compile and correctly read/write enum with StringEnumWriter" in {
    enum A derives StringEnumWriter, StringEnumReader:
      case B, C

    A.B.asTokenList shouldBe TokenNode.value("B")
    A.C.asTokenList shouldBe TokenNode.value("C")

    read[A](
      TokenNode.value("B")
    ) shouldBe A.B

    read[A](
      TokenNode.value("C")
    ) shouldBe A.C
  }

  it should "compile and correctly read/write enum with OrdinalEnumWriter" in {
    enum A derives OrdinalEnumWriter, OrdinalEnumReader:
      case B, C

    A.B.asTokenList shouldBe TokenNode.value(0)
    A.C.asTokenList shouldBe TokenNode.value(1)

    read[A](
      TokenNode.value(0)
    ) shouldBe A.B

    read[A](
      TokenNode.value(1)
    ) shouldBe A.C
  }

  it should "compile and correcly write enum obj with discriminator" in {
    enum A:
      case B, C

    {
      given JsonWriter[A] = StringEnumWriter.withLabel("__type")
      A.B.asTokenList shouldBe obj("__type" -> "B")
      A.C.asTokenList shouldBe obj("__type" -> "C")
    }

    {
      given JsonWriter[A] = OrdinalEnumWriter.withLabel("__type")

      A.B.asTokenList shouldBe obj("__type" -> 0)
      A.C.asTokenList shouldBe obj("__type" -> 1)
    }
  }

  it should "correctly read case classes with default parameters" in {
    object Mod {
      case class WithOpt(x: Int, y: Option[String] = Some("default")) derives JsonReader
    }

    read[Mod.WithOpt](obj("x" -> 5)) shouldBe Mod.WithOpt(5)
  }

  it should "correctly read case classes with default parameters and type arguments" in {
    case class WithArg[A](x: Int, y: Option[A] = None) derives JsonReader

    read[WithArg[Int]](obj("x" -> 5)) shouldBe WithArg[Int](5)
    read[WithArg[String]](obj("x" -> 5, "y" -> "lool")) shouldBe WithArg[String](5, Some("lool"))
  }
}