package tethys.derivation

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.JsonReader
import tethys.commons.*
import tethys.commons.TokenNode.*
import tethys.derivation.RedundantJsonReaderTest.*
import tethys.derivation.builder.ReaderBuilder
import tethys.derivation.semiauto.*
import tethys.readers.tokens.QueueIterator

import tethys.TokenIteratorOps

object RedundantJsonReaderTest {
  case class RedundantClass(i: Int)

  case class BaseClass(r: RedundantClass)
}

class RedundantJsonReaderTest extends AnyFlatSpec with Matchers {
  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  behavior of "jsonReader"
  it should "not require redundant classes for generated readers" in {
    implicit val reader: JsonReader[BaseClass] = jsonReader[BaseClass] {
      describe {
        ReaderBuilder[BaseClass]
          .extract(_.r).from("intField".as[Int])(RedundantClass.apply)
      }
    }

    read[BaseClass](obj(
      "intField" -> 1
    )) shouldBe BaseClass(RedundantClass(1))
  }

}
