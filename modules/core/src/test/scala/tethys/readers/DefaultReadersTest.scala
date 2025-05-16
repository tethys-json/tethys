package tethys.readers

import org.scalatest.matchers.should.Matchers.{value => _, _}
import org.scalatest.flatspec.AnyFlatSpec
import tethys.JsonReader
import tethys.commons.{Token, TokenNode}
import tethys.commons.TokenNode._
import tethys.readers.DefaultReadersTest.TestDefinition
import tethys.readers.tokens._
import tethys._

import scala.reflect.ClassTag

class DefaultReadersTest extends AnyFlatSpec {
  private val randomUUID = java.util.UUID.randomUUID()
  private val instantNow = java.time.Instant.now()
  private val localDateNow = java.time.LocalDate.now()
  private val localDateTimeNow = java.time.LocalDateTime.now()
  private val offsetDateTimeNow = java.time.OffsetDateTime.now()
  private val zonedDateTimeNow = java.time.ZonedDateTime.now()

  private def test[A](
      result: A
  )(implicit jsonReader: JsonReader[A], ct: ClassTag[A]): TestDefinition[A] = {
    TestDefinition(result, jsonReader, ct.toString())
  }

  private def test[A](result: A, name: String)(implicit
      jsonReader: JsonReader[A]
  ): TestDefinition[A] = {
    TestDefinition(result, jsonReader, name)
  }

  private val cases: List[(TestDefinition[_], List[TokenNode])] =
    List[(TestDefinition[_], List[TokenNode])](
      test("1") -> value("1"),
      test('1') -> value("1"),
      test(1) -> value(1),
      test(1: Byte) -> value(1: Byte),
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
      test(Map(randomUUID -> 1), "Map with UUID keys") -> obj(
        randomUUID.toString -> 1
      ),
      test(Map(instantNow -> 1), "Map with Instant keys") -> obj(
        instantNow.toString -> 1
      ),
      test(Map(localDateNow -> 1), "Map with LocalDate keys") ->
        obj(
          localDateNow.format(
            java.time.format.DateTimeFormatter.ISO_LOCAL_DATE
          ) -> 1
        ),
      test(Map(localDateTimeNow -> 1), "Map with LocalDateTime keys") ->
        obj(
          localDateTimeNow.format(
            java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
          ) -> 1
        ),
      test(Map(offsetDateTimeNow -> 1), "Map with OffsetDateTime keys") ->
        obj(
          offsetDateTimeNow.format(
            java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
          ) -> 1
        ),
      test(Map(zonedDateTimeNow -> 1), "Map with ZonedDateTime keys") ->
        obj(
          zonedDateTimeNow.format(
            java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
          ) -> 1
        ),
      test(Map(1L -> 1), "Map with Long keys") -> obj("1" -> 1),
      test(Map(1 -> 1), "Map with Int keys") -> obj("1" -> 1),
      test(Option(1), "Option.nonEmpty") -> value(1),
      test(Option.empty[Int], "Option.empty") -> List(NullValueNode),
      test(1: java.lang.Integer) -> value(1),
      test(java.lang.Byte.valueOf(1: Byte)) -> value(1: Byte),
      test(java.lang.Short.valueOf(1: Short)) -> value(1: Short),
      test(1L: java.lang.Long) -> value(1L),
      test(1f: java.lang.Float) -> value(1f),
      test(1d: java.lang.Double) -> value(1d),
      test(java.math.BigDecimal.valueOf(1)) -> value(1: BigDecimal),
      test(java.math.BigInteger.valueOf(1)) -> value(1: BigInt),
      test(randomUUID) -> value(randomUUID.toString),
      test(instantNow) -> value(instantNow.toString),
      test(localDateNow) -> value(
        localDateNow.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
      ),
      test(localDateTimeNow) -> value(
        localDateTimeNow.format(
          java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME
        )
      ),
      test(offsetDateTimeNow) -> value(
        offsetDateTimeNow.format(
          java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME
        )
      ),
      test(zonedDateTimeNow) -> value(
        zonedDateTimeNow.format(
          java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME
        )
      )
    )

  behavior of "Default readers"

  cases.foreach {
    case (TestDefinition(result, jsonReader, name), nodes) =>
      it should s"correctly read $name" in {
        val iterator = QueueIterator(nodes)
        implicit val reader = jsonReader
        iterator.readJson.fold(throw _, identity) shouldBe result
        iterator.currentToken() shouldBe Token.Empty
      }

    case null =>
      fail("oops")
  }

}

object DefaultReadersTest {
  case class TestDefinition[A](
      result: A,
      jsonReader: JsonReader[A],
      name: String
  )
}
