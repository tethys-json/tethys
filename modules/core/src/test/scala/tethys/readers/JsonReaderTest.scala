package tethys.readers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.{JsonReader, TokenIteratorOps}
import tethys.commons.TokenNode
import tethys.readers.JsonReaderTest.{PositiveInt, Triple}
import tethys.readers.tokens.QueueIterator

class JsonReaderTest extends AnyFlatSpec with Matchers {
  behavior of "JsonReader"

  def read[A: JsonReader](nodes: List[TokenNode]): Either[ReaderError, A] = {
    val it = QueueIterator(nodes)
    it.readJson[A]
  }

  "emap" should "work with base instances" in {
    read[PositiveInt](TokenNode.value(1)) shouldBe Right(
      PositiveInt.fromInt(1).get
    )
    read[PositiveInt](TokenNode.value(42)) shouldBe Right(
      PositiveInt.fromInt(42).get
    )
    read[PositiveInt](TokenNode.value(-1)).left.map(_.getMessage) shouldBe Left(
      "Illegal json at '[ROOT]': value -1 is not positive"
    )
  }

  "emap" should "propagate field names in errors properly" in {
    val one = PositiveInt.fromInt(1).get

    read[Triple](
      TokenNode.obj("fst" -> 1, "snd" -> 1, "thrd" -> 1)
    ) shouldBe Right(Triple(one, one, one))
    read[Triple](TokenNode.obj("fst" -> 1, "snd" -> 13, "thrd" -> 0)).left.map(
      _.getMessage
    ) shouldBe Left("Illegal json at '[ROOT].thrd': value 0 is not positive")
  }
}

object JsonReaderTest {
  class PositiveInt private (val value: Int) extends AnyVal
  object PositiveInt {
    def fromInt(x: Int): Option[PositiveInt] =
      if (x > 0) Some(new PositiveInt(x))
      else None

    implicit val reader: JsonReader[PositiveInt] =
      JsonReader[Int].emap(x =>
        PositiveInt
          .fromInt(x)
          .toRight(
            ReaderError.Details(s"value $x is not positive")
          )
      )
  }

  case class Triple(
      fst: PositiveInt,
      snd: PositiveInt,
      thrd: PositiveInt
  )
  object Triple {
    implicit val reader: JsonReader[Triple] = {
      JsonReader.builder
        .addField[PositiveInt]("fst")
        .addField[PositiveInt]("snd")
        .addField[PositiveInt]("thrd")
        .buildReader(Triple.apply)
    }
  }
}
