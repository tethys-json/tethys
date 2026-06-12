package tethys.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.{Chunk, Stream}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.*
import tethys.readers.ReaderError

sealed trait Event(@selector val kind: String) derives JsonReader
object Event {
  case class Created(id: Int) extends Event("created")
  case class Deleted(id: Int, hard: Boolean) extends Event("deleted")
}

sealed trait ManualEvent
object ManualEvent {
  case class Created(id: Int) extends ManualEvent
  case class Deleted(id: Int, hard: Boolean) extends ManualEvent

  given JsonReader[Created] = JsonReader.derived
  given JsonReader[Deleted] = JsonReader.derived

  given JsonReader[ManualEvent] =
    JsonReader.builder
      .addField[String]("kind")
      .selectReader[ManualEvent] {
        case "created" => summon[JsonReader[Created]]
        case "deleted" => summon[JsonReader[Deleted]]
      }
}

class DiscriminatorResolutionTest extends AnyFlatSpec with Matchers {
  import Event.*

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

  "scala-3 discriminator derivation" should "resolve sum-type branches in decodeDocumentsFromStream" in {
    val chunks = List(
      """{"kind":"created","id":1}{"kind":"dele""",
      """ted","id":2,"hard":true}{"kin""",
      """d":"created","id":3}"""
    )

    val result = JsonReader[Event]
      .decodeDocumentsFromStream[IO](chunkedSource(chunks))
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(Created(1)),
      Right(Deleted(2, hard = true)),
      Right(Created(3))
    )
  }

  it should "resolve sum-type branches in decodeNDocumentsFromStream" in {
    val json =
      """{"kind":"created","id":1}
        |{"kind":"deleted","id":2,"hard":false}
        |{"kind":"created","id":3}""".stripMargin

    JsonReader[Event]
      .decodeNDocumentsFromStream[IO, IO](source(json), 2)
      .unsafeRunSync() shouldBe List(
      Right(Created(1)),
      Right(Deleted(2, hard = false))
    )
  }

  it should "resolve sum-type branches in decodeFromStream for a root array document" in {
    val json =
      """[
        |  {"kind":"created","id":1},
        |  {"kind":"deleted","id":2,"hard":true},
        |  {"kind":"created","id":3}
        |]""".stripMargin

    JsonReader[List[Event]]
      .decodeFromStream(byteArraySource(json))
      .unsafeRunSync() shouldBe List(
      Created(1),
      Deleted(2, hard = true),
      Created(3)
    )
  }

  it should "resolve sum-type branches in Parse.oneDocument for array elements" in {
    val json =
      """{
        |  "items": [
        |    {"kind":"created","id":1},
        |    {"kind":"deleted","id":2,"hard":false},
        |    {"kind":"created","id":3}
        |  ]
        |}""".stripMargin

    val result = source(json)
      .through(Parse.oneDocument("items").everyElementAs[Event].toFs2Stream[IO])
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(Created(1)),
      Right(Deleted(2, hard = false)),
      Right(Created(3))
    )
  }

  it should "resolve branches with a manually provided selectReader-based JsonReader" in {
    val json =
      """{"kind":"created","id":1}
        |{"kind":"deleted","id":2,"hard":true}
        |{"kind":"created","id":3}""".stripMargin

    val result = JsonReader[ManualEvent]
      .decodeDocumentsFromStream[IO](source(json))
      .compile
      .toVector
      .unsafeRunSync()

    normalize(result) shouldBe Vector(
      Right(ManualEvent.Created(1)),
      Right(ManualEvent.Deleted(2, hard = true)),
      Right(ManualEvent.Created(3))
    )
  }
}
