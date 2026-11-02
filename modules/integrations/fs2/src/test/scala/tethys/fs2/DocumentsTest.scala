package tethys.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.fasterxml.jackson.core.JsonParseException
import fs2.{Chunk, Stream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.{JsonReader, _}
import tethys.readers.ReaderError

class DocumentsTest extends AnyFlatSpec with Matchers {
  case class Foo(a: Int)
  object Foo {
    implicit val jsonReader: JsonReader[Foo] = JsonReader.builder
      .addField[Int]("a")
      .buildReader(Foo.apply)
  }

  case class FooWithOptional(a: Int, b: Option[Int])
  object FooWithOptional {
    implicit val jsonReader: JsonReader[FooWithOptional] = JsonReader.builder
      .addField[Int]("a")
      .addField[Option[Int]]("b")
      .buildReader(FooWithOptional.apply)
  }

  private def source(jsonString: String): Stream[IO, Byte] =
    Stream.emits[IO, Byte](jsonString.getBytes("UTF-8"))

  private def byteArraySource(jsonString: String): Stream[IO, Array[Byte]] =
    source(jsonString).chunks.map(_.toArray)

  private def chunkedSource(chunks: List[String]): Stream[IO, Byte] =
    Stream
      .emits[IO, Chunk[Byte]](
        chunks.map(chunk => Chunk.array(chunk.getBytes("UTF-8")))
      )
      .flatMap(Stream.chunk)

  private def normalize[T](result: Vector[Either[ReaderError, T]]) =
    result.map(_.left.map(_.getMessage))

  "decodeDocumentsFromStream" should "decode newline-separated top-level json documents" in {
    val json =
      """{"a":1}
        |{"a":2}
        | {"a":3}""".stripMargin

    val result = JsonReader[Foo]
      .decodeDocumentsFromStream[IO](source(json))
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(Foo(1)),
      Right(Foo(2)),
      Right(Foo(3))
    )
  }

  it should "decode a root array document with decodeFromStream" in {
    val json =
      """[
        |  {"a":1},
        |  {"a":2},
        |  {"a":3}
        |]""".stripMargin

    JsonReader[List[Foo]]
      .decodeFromStream(byteArraySource(json))
      .unsafeRunSync() shouldBe List(Foo(1), Foo(2), Foo(3))
  }

  it should "decode concatenated documents split across irregular chunk boundaries" in {
    val chunks = List(
      """{"a":""",
      """1}{"a""",
      """": 2}{"a":3}"""
    )

    val result = JsonReader[Foo]
      .decodeDocumentsFromStream[IO](chunkedSource(chunks))
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(Foo(1)),
      Right(Foo(2)),
      Right(Foo(3))
    )
  }

  it should "continue after document decoding failures when json syntax is valid" in {
    val json =
      """{"a":1}
        |{"b":2}
        |{"a":3}""".stripMargin

    val result = JsonReader[Foo]
      .decodeDocumentsFromStream[IO](source(json))
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(Foo(1)),
      Left("Illegal json at '[ROOT]': Can not extract fields 'a'"),
      Right(Foo(3))
    )
  }

  it should "fail the stream on malformed json in the middle of the document sequence" in {
    val json =
      """{"a":1}
        |{"a": nope}
        |{"a":3}""".stripMargin

    the[JsonParseException] thrownBy {
      JsonReader[Foo]
        .decodeDocumentsFromStream[IO](source(json))
        .compile
        .toVector
        .unsafeRunSync()
    }
  }

  it should "decode only the requested number of documents with decodeNDocumentsFromStream" in {
    val json =
      """{"a":1}
        |{"a":2}
        |{"a":3}""".stripMargin

    val result = JsonReader[Foo]
      .decodeNDocumentsFromStream[IO, IO](source(json), 2)
      .unsafeRunSync()

    result shouldBe List(Right(Foo(1)), Right(Foo(2)))
  }

  it should "decode documents with missing optional fields" in {
    val json =
      """{"a":1,"b":1}
        |{"a":2}
        |{"a":3,"b":3}""".stripMargin

    val result = JsonReader[FooWithOptional]
      .decodeDocumentsFromStream[IO](source(json))
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(FooWithOptional(1, Some(1))),
      Right(FooWithOptional(2, None)),
      Right(FooWithOptional(3, Some(3)))
    )
  }
}
