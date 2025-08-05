package tethys.readers

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.JsonReader
import tethys.commons.{Token, TokenNode}
import tethys.commons.TokenNode._
import tethys.readers.JsonReaderBuilderTest._
import tethys.readers.tokens.QueueIterator
import tethys.TokenIteratorOps

class JsonReaderBuilderTest extends AnyFlatSpec with Matchers {
  behavior of "JsonReaderBuilder"

  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  it should "build reader from fields" in {
    implicit val reader: JsonReader[B] = {
      JsonReader.builder
        .field[Int]("i")
        .buildReader(i => B(i))
    }

    read[B](obj("i" -> 1)) shouldBe B(1)
  }

  it should "build selecting reader from fields" in {
    implicit val readerB: JsonReader[B] = {
      JsonReader.builder
        .field[Int]("i")
        .buildReader(i => B(i))
    }

    implicit val readerC: JsonReader[C] = {
      JsonReader.builder
        .field[String]("s")
        .buildReader(s => C(s))
    }

    implicit val readerA: JsonReader[A] = {
      JsonReader.builder
        .field[String]("clazz")
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
        .field[Int]("a")
        .field[String]("b")
        .field[Boolean]("c")
        .field[Seq[String]]("d")
        .field[Double]("e")
        .field[Char]("f")
        .field[Option[Int]]("opt")
        .buildReader(FatClass.apply)
    }

    read[FatClass](
      obj(
        "a" -> 1,
        "b" -> "s",
        "c" -> true,
        "d" -> arr("a", "b", "c"),
        "e" -> 4,
        "f" -> "c"
      )
    ) shouldBe FatClass(
      a = 1,
      b = "s",
      c = true,
      d = Seq("a", "b", "c"),
      e = 4.0d,
      f = 'c',
      opt = None
    )
  }

  it should "build strict reader from fields" in {
    implicit val reader: JsonReader[B] = {
      JsonReader.builder
        .field[Option[Int]]("i")
        .buildStrictReader(i => B(i.getOrElse(0)))
    }

    val thrown = the[ReaderError] thrownBy read[B](obj("j" -> 1))
    thrown.getMessage should equal(
      "Illegal json at '[ROOT]': unexpected field 'j', expected one of 'i'"
    )
  }

  it should "allow to build reader with more than 22 fields" in {
    implicit val reader: JsonReader[
      (
          (
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int,
              Int
          ),
          Int,
          Int
      )
    ] = {
      JsonReader.builder
        .field[Int]("f1")
        .field[Int]("f2")
        .field[Int]("f3")
        .field[Int]("f4")
        .field[Int]("f5")
        .field[Int]("f6")
        .field[Int]("f7")
        .field[Int]("f8")
        .field[Int]("f9")
        .field[Int]("f10")
        .field[Int]("f11")
        .field[Int]("f12")
        .field[Int]("f13")
        .field[Int]("f14")
        .field[Int]("f15")
        .field[Int]("f16")
        .field[Int]("f17")
        .field[Int]("f18")
        .field[Int]("f19")
        .field[Int]("f20")
        .field[Int]("f21")
        .field[Int]("f22")
        .field[Int]("f23")
        .field[Int]("f24")
        .buildReader((tuple, f23, f24) => (tuple, f23, f24))
    }

    read(
      obj(
        "f1" -> 1,
        "f2" -> 2,
        "f3" -> 3,
        "f4" -> 4,
        "f5" -> 5,
        "f6" -> 6,
        "f7" -> 7,
        "f8" -> 8,
        "f9" -> 9,
        "f10" -> 10,
        "f11" -> 11,
        "f12" -> 12,
        "f13" -> 13,
        "f14" -> 14,
        "f15" -> 15,
        "f16" -> 16,
        "f17" -> 17,
        "f18" -> 18,
        "f19" -> 19,
        "f20" -> 20,
        "f21" -> 21,
        "f22" -> 22,
        "f23" -> 23,
        "f24" -> 24
      )
    )(reader) shouldBe ((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16,
      17, 18, 19, 20, 21, 22), 23, 24)
  }
}

object JsonReaderBuilderTest {
  trait A
  case class B(i: Int) extends A
  case class C(s: String) extends A

  case class FatClass(
      a: Int,
      b: String,
      c: Boolean,
      d: Seq[String],
      e: Double,
      f: Char,
      opt: Option[Int]
  )
}
