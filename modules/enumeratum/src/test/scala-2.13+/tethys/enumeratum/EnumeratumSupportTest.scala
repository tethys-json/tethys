package tethys.enumeratum

import scala.util.Try

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.EitherValues
import tethys.commons.TokenNode.{value => token, _}
import tethys.readers.{FieldName, ReaderError}
import tethys.writers.tokens.SimpleTokenWriter._

class EnumeratumSupportTest extends AnyFlatSpec with Matchers with EitherValues {
  behavior of "TethysEnum"

  it should "work for encode" in {
    for (entry <- Direction.values) {
      entry.asTokenList shouldBe token(entry.entryName)
    }
  }

  it should "work for decode" in {
    for (entry <- Direction.values) {
      token(entry.entryName).tokensAs[Direction] shouldBe entry
    }
  }

  it should "fail for decode with unknown value" in {
    implicit val field = FieldName().appendFieldName("direction")

    (the [ReaderError] thrownBy obj("direction" -> "Wat").tokensAs[Data]).getMessage shouldBe
      ReaderError.catchNonFatal(ReaderError.wrongJson("Wat is not a member of enum Direction")).left.value.getMessage

    for (json <- List(token(1), token(1.0), token("null"), token(false), obj(), arr())) {
      Try(json.tokensAs[Direction]).toOption shouldBe None
    }
  }


  behavior of "TethysKeyEnum"

  // FIXME Type Inference doesn't work somehow w/o typehint
  val directions: Map[Direction, Int] = Map(
    Direction.Up    -> 1,
    Direction.Down  -> 2,
    Direction.Left  -> 3,
    Direction.Right -> 4
  )

  it should "work for encode" in {
    directions.asTokenList shouldBe obj("Up" -> token(1), "Down" -> token(2), "Left" -> token(3), "Right" -> token(4))
  }

  it should "work for decode" in {
    obj(
      "Up" -> token(1),
      "Down" -> token(2),
      "Left" -> token(3),
      "Right" -> token(4)
    ).tokensAs[Map[Direction, Int]] shouldBe directions
  }
}
