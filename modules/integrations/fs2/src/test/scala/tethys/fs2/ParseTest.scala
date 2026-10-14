package tethys.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.fasterxml.jackson.core.JsonParseException
import fs2.Chunk
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.{JsonReader, _}
import tethys.readers.ReaderError

class ParseTest extends AnyFlatSpec with Matchers {
  "Parse" should "handle all the elements when oneDocument called with simpleSequential json" in {
    val testCase @ (path, jsonString) = json.simpleSequential

    assertStreamResult(readAtOnce(path, jsonString))(expectations(testCase))
    assertStreamResult(readByteByByte(path, jsonString))(expectations(testCase))
  }

  it should "handle all the elements when oneDocument called with nestedRepetetive json" in {
    val testCase @ (path, jsonString) = json.nestedRepetetive

    assertStreamResult(readAtOnce(path, jsonString))(expectations(testCase))
    assertStreamResult(readByteByByte(path, jsonString))(expectations(testCase))
  }

  it should "handle all the elements when oneDocument called with nestedRepetetiveIncludingOtherTags json" in {
    val testCase @ (path, jsonString) = json.nestedRepetetiveIncludingOtherTags

    assertStreamResult(readAtOnce(path, jsonString))(expectations(testCase))
    assertStreamResult(readByteByByte(path, jsonString))(expectations(testCase))
  }

  it should "fail on truncated json" in {
    val path = "root" :: Nil
    val jsonString =
      """|{
         |  "root": [
         |    {"value": 1},
         |    {"value": 2}
         |  ]
         |""".stripMargin

    val atOnceError = the[Throwable] thrownBy {
      readAtOnce(path, jsonString).compile.toVector.unsafeRunSync()
    }
    atOnceError.getMessage should include("Unexpected end")

    val byteByByteError = the[Throwable] thrownBy {
      readByteByByte(path, jsonString).compile.toVector.unsafeRunSync()
    }
    byteByByteError.getMessage should include("Unexpected end")
  }

  it should "fail on malformed json" in {
    val path = "root" :: Nil
    val jsonString =
      """|{
         |  "root": [
         |    {"value": 1},
         |    nope
         |  ]
         |}""".stripMargin

    the[JsonParseException] thrownBy {
      readAtOnce(path, jsonString).compile.toVector.unsafeRunSync()
    }

    the[JsonParseException] thrownBy {
      readByteByByte(path, jsonString).compile.toVector.unsafeRunSync()
    }
  }

  it should "handle elements split across irregular chunk boundaries" in {
    val path = Nil
    val chunks = List(
      """[{"value":""",
      """1},{"value""",
      """": 2}]"""
    )

    assertStreamResult(readChunked(path, chunks))(
      Vector(Right(Foo(1)), Right(Foo(2)))
    )
  }

  case class Foo(value: Int)

  object Foo {
    implicit val jsonReader: JsonReader[Foo] = JsonReader.builder
      .addField[Int]("value")
      .buildReader(Foo.apply)
  }

  object json {
    val simpleSequential =
      ("root" :: Nil) ->
        """|{
           |  "root": [
           |    {"value": 1},
           |    {"value": 2},
           |    {"value": 3},
           |    {"value": 4}
           |  ]
           |}""".stripMargin

    val nestedRepetetive =
      ("root" :: "sub" :: Nil) ->
        """|{
           |  "root": [
           |    {
           |      "sub": [
           |        {"value": 1},
           |        {"value": 2}
           |      ]
           |    },
           |    {
           |      "sub": [
           |        {"value": 3},
           |        {"value": 4}
           |      ]
           |    }
           |  ]
           |}""".stripMargin

    val nestedRepetetiveIncludingOtherTags =
      ("root" :: "sub" :: Nil) ->
        """|{
           |  "root": [
           |    {
           |      "sub": [
           |        {"value": 1},
           |        {"bar": "nope"},
           |        {"value": 2}
           |      ]
           |    },
           |    {
           |      "sub": [
           |        {"value": 3},
           |        {"value": 4}
           |      ]
           |    },
           |    {
           |      "bar": "nope"
           |    },
           |    {
           |      "sub": [
           |        {"value": 5}
           |      ]
           |    },
           |    {
           |      "sub": [
           |        {"bar": "nope"}
           |      ]
           |    }
           |  ]
           |}""".stripMargin
  }

  val expectations = Map(
    json.simpleSequential ->
      Vector(1, 2, 3, 4)
        .map(Foo(_))
        .map(value => Right(value): Either[String, Foo]),
    json.nestedRepetetive ->
      Vector(1, 2, 3, 4)
        .map(Foo(_))
        .map(value => Right(value): Either[String, Foo]),
    json.nestedRepetetiveIncludingOtherTags ->
      Vector(
        Right(Foo(1)),
        Left("Illegal json at '[ROOT]': Can not extract fields 'value'"),
        Right(Foo(2)),
        Right(Foo(3)),
        Right(Foo(4)),
        Right(Foo(5)),
        Left("Illegal json at '[ROOT]': Can not extract fields 'value'")
      )
  )

  private def streamBuilder(path: List[String]): Parse.OneDocument =
    path match {
      case head :: tail => tail.foldLeft(Parse.oneDocument(head))(_.inField(_))
      case Nil          => Parse.oneDocument
    }

  private def source(jsonString: String): Stream[IO, Byte] =
    Stream.emits[IO, Byte](jsonString.getBytes("UTF-8"))

  private def chunkedSource(chunks: List[String]): Stream[IO, Byte] =
    Stream
      .emits[IO, Chunk[Byte]](
        chunks.map(chunk => Chunk.array(chunk.getBytes("UTF-8")))
      )
      .flatMap(Stream.chunk)

  def readAtOnce(path: List[String], jsonString: String) = {
    source(jsonString).through(
      streamBuilder(path).everyElementAs[Foo].toFs2Stream[IO]
    )
  }

  def readByteByByte(path: List[String], jsonString: String) = {
    source(jsonString)
      .chunkLimit(1)
      .unchunks
      .through(streamBuilder(path).everyElementAs[Foo].toFs2Stream[IO])
  }

  def readChunked(path: List[String], chunks: List[String]) = {
    chunkedSource(chunks).through(
      streamBuilder(path).everyElementAs[Foo].toFs2Stream[IO]
    )
  }

  def assertStreamResult(stream: Stream[IO, Either[ReaderError, Foo]])(
      expects: Vector[Either[String, Foo]]
  ) = stream.compile.toVector
    .unsafeRunSync()
    .map(_.left.map(_.getMessage)) shouldBe expects
}
