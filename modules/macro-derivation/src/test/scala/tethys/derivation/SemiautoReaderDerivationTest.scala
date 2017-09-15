package tethys.derivation

import org.scalatest.{FlatSpec, Matchers}
import tethys.JsonReader
import tethys.derivation.semiauto._
import tethys.readers.ReaderError
import tethys.readers.tokens.QueueIterator.TokenNode
import tethys.readers.tokens.SimpleToken._
import tethys.readers.tokens.{QueueIterator, Token}

class SemiautoReaderDerivationTest extends FlatSpec with Matchers {

  def read[A: JsonReader](nodes: List[TokenNode]): Either[ReaderError, A] = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A]
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
    )) shouldBe Right(JsonTreeTestData(
      a = 1,
      b = true,
      c = C(D(2))
    ))
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
    )) shouldBe Right(RecursiveType(1, Seq(RecursiveType(2), RecursiveType(3))))

  }

  it should "derive writer for A => B => A cycle" in {
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
    )) shouldBe Right(ComplexRecursionA(1, Some(ComplexRecursionB(2, ComplexRecursionA(3, None)))))
  }
}
