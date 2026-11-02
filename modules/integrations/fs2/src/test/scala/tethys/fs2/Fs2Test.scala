package tethys.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.fasterxml.jackson.core.JsonParseException
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.{JsonReader, _}

class Fs2Test extends AnyFlatSpec with Matchers {
  case class Bar(txt: Int)
  object Bar {
    implicit val jsonReader: JsonReader[Bar] = JsonReader.builder
      .addField[Int]("txt")
      .buildReader(Bar.apply)
  }

  case class Foo(qux: Int, maybeBar: Option[Bar], bars: List[Bar])
  object Foo {
    implicit val jsonReader: JsonReader[Foo] = JsonReader.builder
      .addField[Int]("qux")
      .addField[Option[Bar]]("maybeBar")
      .addField[List[Bar]]("bars")
      .buildReader(Foo.apply)
  }

  private def byteArrayStream(json: String): Stream[IO, Array[Byte]] =
    Stream.iterable[IO, Array[Byte]](json.map(ch => Array(ch.toByte)))

  "Fs2 decoder" should "decode case classes correctly" in {
    val json =
      """{
        |  "qux": 1234,
        |  "bars": [{"txt": 2}, {"txt": 3}],
        |  "maybeBar": {"txt": 1}
        |}""".stripMargin

    val expected = Foo(1234, Some(Bar(1)), List(Bar(2), Bar(3)))
    val stream = byteArrayStream(json)

    JsonReader[Foo].decodeFromStream(stream).unsafeRunSync() shouldBe expected
  }

  it should "fail on truncated json" in {
    val json =
      """{
        |  "qux": 1234,
        |  "bars": [{"txt": 2}, {"txt": 3}],
        |  "maybeBar": {"txt": 1}
        |""".stripMargin

    val stream = byteArrayStream(json)

    val error = the[Throwable] thrownBy {
      JsonReader[Foo].decodeFromStream(stream).unsafeRunSync()
    }

    error.getMessage should include("Unexpected end")
  }

  it should "fail on malformed json" in {
    val json =
      """{
        |  "qux": nope,
        |  "bars": [{"txt": 2}, {"txt": 3}],
        |  "maybeBar": {"txt": 1}
        |}""".stripMargin

    val stream = byteArrayStream(json)

    the[JsonParseException] thrownBy {
      JsonReader[Foo].decodeFromStream(stream).unsafeRunSync()
    }
  }
}
