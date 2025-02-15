package tethys.derivation

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.JsonReader
import tethys.commons.{Token, TokenNode}
import tethys.commons.TokenNode._
import tethys.derivation.auto._
import tethys.readers.tokens.QueueIterator
import tethys._

class AutoReaderDerivationTest extends AnyFlatSpec with Matchers {

  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  behavior of "auto derivation"
  it should "derive readers for simple case class hierarchy" in {
    read[JsonTreeTestData](
      obj(
        "a" -> 1,
        "b" -> true,
        "c" -> obj(
          "d" -> obj(
            "a" -> 2
          )
        )
      )
    ) shouldBe JsonTreeTestData(
      a = 1,
      b = true,
      c = C(D(2))
    )
  }
}
