package tethys

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode
import tethys.readers.ReaderError
import tethys.readers.tokens.{QueueIterator, TokenIterator}

class JsonStreamingTest extends AnyFlatSpec with Matchers {
  behavior of "JsonStreaming"

  private def iteratorOf(elems: Any*): TokenIterator^ =
    QueueIterator(TokenNode.arr(elems*))

  "foldJsonStream" should "decode top-level array values" in {
    val it = iteratorOf(1, 2, 3)

    JsonStreaming.foldJsonStream[Int, List[Int]](it, List.empty[Int]) {
      (acc, value) => acc :+ value
    } shouldBe List(1, 2, 3)

    it.currentToken().isEmpty shouldBe true
  }

  it should "return the initial state for an empty array" in {
    val it = iteratorOf()

    JsonStreaming.foldJsonStream[Int, Int](it, 42) { (acc, value) =>
      acc + value
    } shouldBe 42

    it.currentToken().isEmpty shouldBe true
  }

  it should "report array indices in decode errors" in {
    val it = iteratorOf(1, "boom")

    val err = the[ReaderError] thrownBy {
      JsonStreaming.foldJsonStream[Int, List[Int]](it, List.empty[Int]) {
        (acc, value) => acc :+ value
      }
    }

    err.getMessage should include("[1]")
  }

  it should "reject non-array roots" in {
    val it: TokenIterator^ = QueueIterator(TokenNode.obj("value" -> 1))

    val err = the[ReaderError] thrownBy {
      JsonStreaming.foldJsonStream[Int, List[Int]](it, List.empty[Int]) {
        (acc, value) => acc :+ value
      }
    }

    err.getMessage should include("Expected array start")
  }

  "foreachDecoded" should "visit each decoded value in order" in {
    val it = iteratorOf(1, 2, 3)
    val values = List.newBuilder[Int]

    JsonStreaming.foreachDecoded[Int](it) { value =>
      values += value
    }

    values.result() shouldBe List(1, 2, 3)
    it.currentToken().isEmpty shouldBe true
  }

  it should "do nothing for an empty array" in {
    val it = iteratorOf()
    val values = List.newBuilder[Int]

    JsonStreaming.foreachDecoded[Int](it) { value =>
      values += value
    }

    values.result() shouldBe Nil
    it.currentToken().isEmpty shouldBe true
  }

  "withJsonCursor" should "decode values through a scoped cursor" in {
    val it = iteratorOf(1, 2, 3)

    val prefix = JsonStreaming.withJsonCursor[Int, List[Int]](it) { cursor =>
      List(cursor.pull(), cursor.pull()).flatten
    }

    prefix shouldBe List(1, 2)
  }

  it should "return None immediately for an empty array" in {
    val it = iteratorOf()

    val first = JsonStreaming.withJsonCursor[Int, Option[Int]](it) { cursor =>
      cursor.pull()
    }

    first shouldBe None
    it.currentToken().isEmpty shouldBe true
  }

  it should "leave remaining elements unread when the callback stops early" in {
    val it = iteratorOf(1, 2, 3)

    val prefix = JsonStreaming.withJsonCursor[Int, List[Int]](it) { cursor =>
      List(cursor.pull(), cursor.pull()).flatten
    }

    prefix shouldBe List(1, 2)
    it.currentToken().isNumberValue shouldBe true
    it.int() shouldBe 3
  }
}
