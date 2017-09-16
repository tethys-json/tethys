package tethys.derivation

import org.scalatest.{FlatSpec, Matchers}
import tethys.JsonReader
import tethys.derivation.auto._
import tethys.readers.ReaderError
import tethys.readers.tokens.QueueIterator.TokenNode
import tethys.readers.tokens.SimpleToken._
import tethys.readers.tokens.{QueueIterator, Token}

class AutoReaderDerivationTest extends FlatSpec with Matchers {

  def read[A: JsonReader](nodes: List[TokenNode]): Either[ReaderError, A] = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A]
    it.currentToken() shouldBe Token.Empty
    res
  }


  behavior of "auto derivation"
  it should "derive readers for simple case class hierarchy" in {
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

  it should "derive writer for A => B => A cycle" in {
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
}
