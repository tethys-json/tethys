package tethys.jackson

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys._
import tethys.commons.RawJson
import tethys.writers.tokens.TokenWriter

class RawJsonTest extends AnyFlatSpec with Matchers {
  behavior of "RawJson.reader"
  it should "read int values as is" in {
    "123".jsonAs[RawJson] shouldBe Right(RawJson("123"))
  }
  it should "read double values with .0 as is" in {
    "123.0".jsonAs[RawJson] shouldBe Right(RawJson("123.0"))
  }
  it should "read double values as is" in {
    "123.1".jsonAs[RawJson] shouldBe Right(RawJson("123.1"))
  }
  it should "read boolean values as is" in {
    "true".jsonAs[RawJson] shouldBe Right(RawJson("true"))
    "false".jsonAs[RawJson] shouldBe Right(RawJson("false"))
  }
  it should "read string values as is" in {
    "\"string\"".jsonAs[RawJson] shouldBe Right(RawJson("\"string\""))
  }
  it should "read null values as is" in {
    "null".jsonAs[RawJson] shouldBe Right(RawJson("null"))
  }
  it should "read arrays" in {
    "[1, 2, true,3.0,\"a\"]".jsonAs[RawJson] shouldBe Right(
      RawJson("[1,2,true,3.0,\"a\"]")
    )
  }
  it should "read objects" in {
    """
      {
        "a": 1,
        "b": false,
        "c": "d",
        "e": { "f": null },
        "g": [1,2,3]
      }
    """.jsonAs[RawJson] shouldBe Right(
      RawJson("""{"a":1,"b":false,"c":"d","e":{"f":null},"g":[1,2,3]}""")
    )
  }

  behavior of "RawJson.writer"
  it should "write json as is from root" in {
    val json =
      """{
        "a": 1,
        "b": false,
        "c": "d",
        "e": { "f": null },
        "g": [1,2,3]
      }"""
    RawJson(json).asJson shouldBe json
  }
  it should "write json as is in middle of object" in {
    case class Foo(a: Int, b: RawJson, c: Boolean)
    implicit val fooWriter: JsonWriter[Foo] = JsonWriter
      .obj[Foo]
      .addField("a")(_.a)
      .addField("b")(_.b)
      .addField("c")(_.c)

    val json =
      """{
        "a": 1,
        "b": false,
        "c": "d",
        "e": { "f": null },
        "g": [1,2,3]
      }"""

    Foo(1, RawJson(json), false).asJson shouldBe """{"a":1,"b":{
        "a": 1,
        "b": false,
        "c": "d",
        "e": { "f": null },
        "g": [1,2,3]
      },"c":false}"""
  }
  it should "write json as is in middle of array" in {
    val writer: JsonWriter[List[Any]] = new JsonWriter[List[Any]] {
      override def write(value: List[Any], tokenWriter: TokenWriter): Unit = {
        tokenWriter.writeArrayStart()
        JsonWriter.intWriter.write(value(0).asInstanceOf[Int], tokenWriter)
        RawJson.rawJsonWriter.write(value(1).asInstanceOf[RawJson], tokenWriter)
        JsonWriter.booleanWriter.write(
          value(2).asInstanceOf[Boolean],
          tokenWriter
        )
        tokenWriter.writeArrayEnd()
      }
    }

    val json =
      """{
        "a": 1,
        "b": false,
        "c": "d",
        "e": { "f": null },
        "g": [1,2,3]
      }"""

    List[Any](1, RawJson(json), false).asJsonWith(writer) shouldBe """[1,{
        "a": 1,
        "b": false,
        "c": "d",
        "e": { "f": null },
        "g": [1,2,3]
      },false]"""
  }
}
