package tethys.derivation

import org.scalatest.{FlatSpec, Matchers}
import tethys.JsonReader
import tethys.commons.TokenNode._
import tethys.commons.{Token, TokenNode}
import tethys.derivation.builder.ReaderBuilder
import tethys.derivation.semiauto._
import tethys.readers.tokens.QueueIterator

class SemiautoReaderDerivationTest extends FlatSpec with Matchers {

  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }


  behavior of "semiauto derivation"
  it should "derive readers for simple case class hierarchy" in {
    implicit val dReader: JsonReader[D] = jsonReader[D]
    implicit val cReader: JsonReader[C] = jsonReader[C]
    implicit val jsonTreeTestDataReader: JsonReader[JsonTreeTestData] = jsonReader[JsonTreeTestData]

    read[JsonTreeTestData](obj(
      "a" -> 1,
      "b" -> true,
      "c" -> obj(
        "d" -> obj(
          "a" -> 2
        )
      )
    )) shouldBe JsonTreeTestData(
      a = 1,
      b = true,
      c = C(D(2))
    )
  }

  it should "derive reader for recursive type" in {
    implicit lazy val recursiveReader: JsonReader[RecursiveType] = jsonReader[RecursiveType]

    read[RecursiveType](obj(
      "a" -> 1,
      "children" -> arr(
        obj(
          "a" -> 2,
          "children" -> arr()
        ),
        obj(
          "a" -> 3,
          "children" -> arr()
        )
      )
    )) shouldBe RecursiveType(1, Seq(RecursiveType(2), RecursiveType(3)))

  }

  it should "derive reader for A => B => A cycle" in {
    implicit lazy val testReader1: JsonReader[ComplexRecursionA] = jsonReader[ComplexRecursionA]
    implicit lazy val testReader2: JsonReader[ComplexRecursionB] = jsonReader[ComplexRecursionB]

    read[ComplexRecursionA](obj(
      "a" -> 1,
      "b" -> obj(
        "b" -> 2,
        "a" -> obj(
          "a" -> 3
        )
      )
    )) shouldBe ComplexRecursionA(1, Some(ComplexRecursionB(2, ComplexRecursionA(3, None))))
  }

  it should "derive reader for extract as description" in {
    implicit val reader: JsonReader[SimpleType] = jsonReader[SimpleType] {
      describe {
        ReaderBuilder[SimpleType]
          .extract(_.i).as[Option[Int]](_.getOrElse(2))
      }
    }

    read[SimpleType](obj(
      "i" -> 1,
      "s" -> "str",
      "d" -> 1.0
    )) shouldBe SimpleType(1, "str", 1.0)

    read[SimpleType](obj(
      "s" -> "str",
      "d" -> 1.0
    )) shouldBe SimpleType(2, "str", 1.0)
  }

  it should "derive reader for extract from description" in {
    implicit val reader: JsonReader[SimpleType] = jsonReader[SimpleType] {
      describe {
        ReaderBuilder[SimpleType]
          .extract(_.i).from(_.s, _.d)((s, d) => 2)
      }
    }

    read[SimpleType](obj(
      "i" -> 1,
      "s" -> "str",
      "d" -> 1.0
    )) shouldBe SimpleType(2, "str", 1.0)

    read[SimpleType](obj(
      "s" -> "str",
      "d" -> 1.0
    )) shouldBe SimpleType(2, "str", 1.0)
  }

  it should "derive reader for extract from description with synthetic field" in {
    implicit val reader: JsonReader[SimpleType] = jsonReader[SimpleType] {
      describe {
        ReaderBuilder[SimpleType]
          .extract(_.i).from(_.d).and('e.as[Double])((d, e) => (d + e).toInt)
      }
    }

    read[SimpleType](obj(
      "i" -> 1,
      "s" -> "str",
      "d" -> 1.0,
      "e" -> 2.0
    )) shouldBe SimpleType(3, "str", 1.0)

    read[SimpleType](obj(
      "s" -> "str",
      "d" -> 1.0,
      "e" -> 3.0
    )) shouldBe SimpleType(4, "str", 1.0)
  }

  it should "derive reader for extract reader from description" in {
    implicit val reader: JsonReader[SimpleTypeWithAny] = jsonReader[SimpleTypeWithAny] {
      describe {
        ReaderBuilder[SimpleTypeWithAny]
          .extractReader(_.any).from(_.d) {
          case 1.0 => JsonReader[String]
          case 2.0 => JsonReader[Int]
        }
      }
    }

    read[SimpleTypeWithAny](obj(
      "i" -> 1,
      "s" -> "str",
      "d" -> 1.0,
      "any" -> "anyStr"
    )) shouldBe SimpleTypeWithAny(1, "str", 1.0, "anyStr")

    read[SimpleTypeWithAny](obj(
      "i" -> 1,
      "s" -> "str",
      "d" -> 2.0,
      "any" -> 2
    )) shouldBe SimpleTypeWithAny(1, "str", 2.0, 2)
  }

  it should "derive reader for complex extraction case" in {
    implicit val reader: JsonReader[SimpleTypeWithAny] = jsonReader[SimpleTypeWithAny] {
      describe {
        ReaderBuilder[SimpleTypeWithAny]
          .extractReader(_.any).from(_.i) {
            case 1 => JsonReader[String]
            case 2 => JsonReader[Int]
            case _ => JsonReader[Option[Boolean]]
          }
          .extract(_.i).from(_.d).and('e.as[Int])((d, e) => d.toInt + e)
          .extract(_.d).as[Option[Double]](_.getOrElse(1.0))
      }
    }

    read[SimpleTypeWithAny](obj(
      "s" -> "str",
      "d" -> 1.0,
      "e" -> 0,
      "any" -> "anyStr"
    )) shouldBe SimpleTypeWithAny(1, "str", 1.0, "anyStr")

    read[SimpleTypeWithAny](obj(
      "s" -> "str",
      "e" -> 0,
      "any" -> "anyStr"
    )) shouldBe SimpleTypeWithAny(1, "str", 1.0, "anyStr")

    read[SimpleTypeWithAny](obj(
      "s" -> "str",
      "d" -> 1.0,
      "e" -> 1,
      "any" -> 3
    )) shouldBe SimpleTypeWithAny(2, "str", 1.0, 3)

    read[SimpleTypeWithAny](obj(
      "s" -> "str",
      "d" -> 1.0,
      "e" -> 2,
      "any" -> true
    )) shouldBe SimpleTypeWithAny(3, "str", 1.0, Some(true))

    read[SimpleTypeWithAny](obj(
      "s" -> "str",
      "d" -> 1.0,
      "e" -> 2
    )) shouldBe SimpleTypeWithAny(3, "str", 1.0, None)
  }
}
