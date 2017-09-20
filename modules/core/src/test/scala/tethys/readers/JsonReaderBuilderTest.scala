package tethys.readers

import org.scalatest.{FlatSpec, Matchers}
import tethys.JsonReader
import tethys.commons.{Token, TokenNode}
import tethys.commons.TokenNode._
import tethys.readers.JsonReaderBuilderTest._
import tethys.readers.tokens.QueueIterator

class JsonReaderBuilderTest extends FlatSpec with Matchers {
  behavior of "JsonReaderBuilder"

  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A]
    it.currentToken() shouldBe Token.Empty
    res.right.get
  }

  it should "build reader from fields" in {
    implicit val reader: JsonReader[B] = {
      JsonReader.builder
        .addField[Int]("i")
        .buildReader(i => B(i))
    }

    read[B](obj("i" -> 1)) shouldBe B(1)
  }

  it should "build selecting reader from fields" in {
    implicit val readerB: JsonReader[B] = {
      JsonReader.builder
        .addField[Int]("i")
        .buildReader(i => B(i))
    }

    implicit val readerC: JsonReader[C] = {
      JsonReader.builder
        .addField[String]("s")
        .buildReader(s => C(s))
    }

    implicit val readerA: JsonReader[A] = {
      JsonReader.builder
        .addField[String]("clazz")
        .selectReader[A] {
          case "B" => readerB
          case "C" => readerC
        }
    }

    read[A](obj("clazz" -> "B", "i" -> 2)) shouldBe B(2)
    read[A](obj("s" -> "str", "clazz" -> "C")) shouldBe C("str")
  }

  it should "build reader for fat object" in {
    implicit val reader: JsonReader[FatClass] = {
      JsonReader.builder
        .addField[Int]("a")
        .addField[String]("b")
        .addField[Boolean]("c")
        .addField[Seq[String]]("d")
        .addField[Double]("e")
        .addField[Option[Int]]("opt")
        .buildReader(FatClass.apply)
    }

    read[FatClass](obj(
      "a" -> 1,
      "b" -> "s",
      "c" -> true,
      "d" -> arr("a", "b", "c"),
      "e" -> 4
    )) shouldBe FatClass(
      a = 1,
      b = "s",
      c = true,
      d = Seq("a", "b", "c"),
      e = 4.0D,
      opt = None
    )
  }
}

object JsonReaderBuilderTest {
  trait A
  case class B(i: Int) extends A
  case class C(s: String) extends A

  case class FatClass(a: Int,
                      b: String,
                      c: Boolean,
                      d: Seq[String],
                      e: Double,
                      opt: Option[Int])
}
