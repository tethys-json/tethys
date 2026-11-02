package tethys.fs2

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.{JsonReader, JsonWriter, _}
import tethys.jackson._

class WriteTest extends AnyFlatSpec with Matchers {
  case class Foo(a: Int)

  object Foo {
    implicit val jsonWriter: JsonWriter[Foo] =
      JsonWriter.obj[Foo].addField("a")(_.a)

    implicit val jsonReader: JsonReader[Foo] = JsonReader.builder
      .addField[Int]("a")
      .buildReader(Foo.apply)
  }

  "asJsonArrayByteStream" should "write a stream of values as one json array document" in {
    val bytes = Stream
      .emits[IO, Foo](List(Foo(1), Foo(2), Foo(3)))
      .asJsonArrayByteStream()
      .compile
      .toVector
      .unsafeRunSync()
      .toArray

    new String(bytes, "UTF-8") shouldBe """[{"a":1},{"a":2},{"a":3}]"""

    JsonReader[List[Foo]]
      .decodeFromStream[IO, IO](Stream.emit(bytes))
      .unsafeRunSync() shouldBe List(Foo(1), Foo(2), Foo(3))
  }

  it should "write an empty stream as an empty json array document" in {
    val bytes = Stream
      .emits[IO, Foo](Nil)
      .asJsonArrayByteStream()
      .compile
      .toVector
      .unsafeRunSync()
      .toArray

    new String(bytes, "UTF-8") shouldBe "[]"
  }
}
