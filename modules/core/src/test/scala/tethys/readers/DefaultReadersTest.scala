package tethys.readers

import org.scalatest.FlatSpec
import org.scalatest.Matchers.{value => _, _}
import tethys.JsonReader
import tethys.readers.DefaultReadersTest.TestDefinition
import tethys.readers.tokens.QueueIterator.TokenNode
import tethys.readers.tokens.SimpleToken._
import tethys.readers.tokens._

import scala.reflect.ClassTag

class DefaultReadersTest extends FlatSpec {

  private def test[A](result: A)(implicit jsonReader: JsonReader[A], ct: ClassTag[A]): TestDefinition[A] = {
    TestDefinition(result, jsonReader, ct.toString())
  }

  private def test[A](result: A, name: String)(implicit jsonReader: JsonReader[A]): TestDefinition[A] = {
    TestDefinition(result, jsonReader, name)
  }

  private val cases: List[(TestDefinition[_], List[TokenNode])] = List[(TestDefinition[_], List[TokenNode])](
    test("1") -> value("1"),
    test('1') -> value("1"),
    test(1) -> value(1),
    test(1: Short) -> value(1: Short),
    test(1L) -> value(1L),
    test(1f) -> value(1f),
    test(1d) -> value(1d),
    test(1: BigDecimal) -> value(1: BigDecimal),
    test(true, "true") -> value(true),
    test(false, "false") -> value(false),
    test(List(1, 2, 3)) -> arr(1, 2, 3),
    test(List[Int](), "Seq.empty") -> arr(),
    test(Map("a" -> 1, "b" -> 2)) -> obj("a" -> 1, "b" -> 2),
    test(Option(1), "Option.nonEmpty") -> value(1),
    test(Option.empty[Int], "Option.empty") -> List(TokenNode(NullValueToken)),
    test(1: java.lang.Integer) -> value(1),
    test(java.lang.Short.valueOf(1: Short)) -> value(1: Short),
    test(1L: java.lang.Long) -> value(1L),
    test(1f: java.lang.Float) -> value(1f),
    test(1d: java.lang.Double) -> value(1d),
    test(java.math.BigDecimal.valueOf(1)) -> value(1: BigDecimal),
    test(java.math.BigInteger.valueOf(1)) -> value(1: BigInt)
  )

  behavior of "Default readers"

  cases.foreach {
    case (TestDefinition(result, jsonReader, name), nodes) =>
      it should s"correctly read $name" in {
        val iterator = QueueIterator(nodes)
        iterator.readJson(jsonReader) shouldBe Right(result)
        iterator.currentToken() shouldBe Token.Empty
      }

    case _ =>
      fail("oops")
  }


}

object DefaultReadersTest {
  case class TestDefinition[A](result: A, jsonReader: JsonReader[A], name: String)
}
