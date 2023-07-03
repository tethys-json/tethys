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



  it should "build strict reader from fields" in {
    implicit val reader: JsonReader[B] = {
      JsonReader.builder
        .addField[Option[Int]]("i")
        .buildStrictReader(i => B(i.getOrElse(0)))
    }

    val thrown = the [ReaderError] thrownBy read[B](obj("j" -> 1))
    thrown.getMessage should equal ("Illegal json at '[ROOT]': unexpected field 'j', expected one of 'i'")
  }

  it should "allow to build reader with more than 22 fields" in {
    implicit val reader: JsonReader[((Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int), Int, Int)] = {
      JsonReader.builder
        .addField[Int]("f1")
        .addField[Int]("f2")
        .addField[Int]("f3")
        .addField[Int]("f4")
        .addField[Int]("f5")
        .addField[Int]("f6")
        .addField[Int]("f7")
        .addField[Int]("f8")
        .addField[Int]("f9")
        .addField[Int]("f10")
        .addField[Int]("f11")
        .addField[Int]("f12")
        .addField[Int]("f13")
        .addField[Int]("f14")
        .addField[Int]("f15")
        .addField[Int]("f16")
        .addField[Int]("f17")
        .addField[Int]("f18")
        .addField[Int]("f19")
        .addField[Int]("f20")
        .addField[Int]("f21")
        .addField[Int]("f22")
        .addField[Int]("f23")
        .addField[Int]("f24")
        .buildReader((tuple, f23, f24) => (tuple, f23, f24))
    }

    read(obj(
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
    ))(reader) shouldBe ((1 ,2 ,3 ,4 ,5 ,6 ,7 ,8 ,9 ,10 ,11 ,12 ,13 ,14 ,15 ,16 ,17 ,18 ,19 ,20 ,21 ,22), 23, 24)
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
