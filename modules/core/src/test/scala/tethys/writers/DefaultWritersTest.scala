package tethys.writers

import org.scalatest.matchers.should.Matchers.{value => _, _}
import org.scalatest.flatspec.AnyFlatSpec
import tethys.JsonWriter
import tethys.commons.TokenNode
import tethys.commons.TokenNode._
import tethys.writers.DefaultWritersTest.TestDefinition
import tethys.writers.tokens.SimpleTokenWriter._

import scala.reflect.ClassTag

class DefaultWritersTest extends AnyFlatSpec {
  private val randomUUID = java.util.UUID.randomUUID()

  private def test[A](
      value: A
  )(implicit jsonWriter: JsonWriter[A], ct: ClassTag[A]): TestDefinition[A] = {
    TestDefinition(value, jsonWriter, ct.toString())
  }

  private def test[A](value: A, name: String)(implicit
      jsonWriter: JsonWriter[A]
  ): TestDefinition[A] = {
    TestDefinition(value, jsonWriter, name)
  }

  private val cases: List[(TestDefinition[_], List[TokenNode])] =
    List[(TestDefinition[_], List[TokenNode])](
      test("1") -> value("1"),
      test(1) -> value(1),
      test(1: Short) -> value(1: Short),
      test(1L) -> value(1L),
      test(1f) -> value(1f),
      test(1d) -> value(1d),
      test(1: BigDecimal) -> value(1: BigDecimal),
      test(1: BigInt) -> value(1: BigInt),
      test(true, "true") -> value(true),
      test(false, "false") -> value(false),
      test(List(1, 2, 3)) -> arr(1, 2, 3),
      test(List[Int](), "Seq.empty") -> arr(),
      test(Map("a" -> 1, "b" -> 2)) -> obj("a" -> 1, "b" -> 2),
      test(Map(randomUUID -> 1), "Map with UUID keys") -> obj(
        randomUUID.toString -> 1
      ),
      test(Map(1L -> 1), "Map with Long keys") -> obj("1" -> 1),
      test(Map(1 -> 1), "Map with Int keys") -> obj("1" -> 1),
      test(Option(1), "Option.nonEmpty") -> value(1),
      test(Option.empty[Int], "Option.empty") -> List(NullValueNode),
      test(Right(1): Either[String, Int], "Either.right") -> value(1),
      test(Left("Not an Int"): Either[String, Int], "Either.left") -> value(
        "Not an Int"
      ),
      test(1: java.lang.Integer) -> value(1),
      test(java.lang.Short.valueOf(1: Short)) -> value(1: Short),
      test(1L: java.lang.Long) -> value(1L),
      test(1f: java.lang.Float) -> value(1f),
      test(1d: java.lang.Double) -> value(1d),
      test(java.math.BigDecimal.valueOf(1)) -> value(1: BigDecimal),
      test(java.math.BigInteger.valueOf(1)) -> value(1: BigInt),
      test(randomUUID) -> value(randomUUID.toString)
    )

  behavior of "Default writers"

  cases.foreach {
    case (TestDefinition(value, jsonWriter, name), result) =>
      it should s"correctly write $name" in {
        value.asTokenList(jsonWriter) shouldBe result
      }

    case null =>
      fail("oops")
  }

}

object DefaultWritersTest {
  case class TestDefinition[A](
      value: A,
      jsonWriter: JsonWriter[A],
      name: String
  )
}
