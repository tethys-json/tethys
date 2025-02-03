package tethys.writers.tokens

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

abstract class TokenWriterSpec(implicit producer: TokenWriterProducer)
    extends AnyFlatSpecLike
    with Matchers {

  behavior of "TokenWriter"

  def testCase(
      json: String,
      iterate: TokenWriter => Unit
  ): Unit = {
    it should s"write $json" in {
      val writer = producer.produce(TokenWriterConfig.default)
      iterate(writer)
      writer.flush()
      writer.result() shouldBe json
    }
  }

  testCase(
    json = """true""",
    iterate = _.writeBoolean(true)
  )

  testCase(
    json = """false""",
    iterate = _.writeBoolean(false)
  )

  testCase(
    json = """null""",
    iterate = _.writeNull()
  )

  testCase(
    json = """"hello"""",
    iterate = _.writeString("hello")
  )

  testCase(
    json = """"special chars: \"\\\n\r\t\b\f"""",
    iterate = _.writeString("special chars: \"\\\n\r\t\b\f")
  )

  testCase(
    json = """42""",
    iterate = _.writeNumber(42)
  )

  testCase(
    json = """42.5""",
    iterate = _.writeNumber(42.5)
  )

  testCase(
    json = """9223372036854775807""",
    iterate = _.writeNumber(Long.MaxValue)
  )

  testCase(
    json = """3.141592653589793""",
    iterate = _.writeNumber(Math.PI)
  )

  testCase(
    json = """1.23456789E10""",
    iterate = _.writeNumber(1.23456789e+10)
  )

  testCase(
    json = """{}""",
    iterate = _.writeObjectStart().writeObjectEnd()
  )

  testCase(
    json = """[]""",
    iterate = _.writeArrayStart().writeArrayEnd()
  )

  testCase(
    json = """{"name":"John","age":30}""",
    iterate = writer => {
      writer.writeObjectStart()
      writer.writeFieldName("name")
      writer.writeString("John")
      writer.writeFieldName("age")
      writer.writeNumber(30)
      writer.writeObjectEnd()
    }
  )

  testCase(
    json = """[1,2,3,4,5]""",
    iterate = writer => {
      writer.writeArrayStart()
      for (i <- 1 to 5) writer.writeNumber(i)
      writer.writeArrayEnd()
    }
  )

  testCase(
    json = """[1,"two",true,null,3.14]""",
    iterate = writer => {
      writer.writeArrayStart()
      writer.writeNumber(1)
      writer.writeString("two")
      writer.writeBoolean(true)
      writer.writeNull()
      writer.writeNumber(3.14)
      writer.writeArrayEnd()
    }
  )

  testCase(
    json =
      """{"person":{"name":"John","age":30,"address":{"city":"New York","zip":"10001"}}}""",
    iterate = writer => {
      writer.writeObjectStart()
      writer.writeFieldName("person")
      writer.writeObjectStart()
      writer.writeFieldName("name")
      writer.writeString("John")
      writer.writeFieldName("age")
      writer.writeNumber(30)
      writer.writeFieldName("address")
      writer.writeObjectStart()
      writer.writeFieldName("city")
      writer.writeString("New York")
      writer.writeFieldName("zip")
      writer.writeString("10001")
      writer.writeObjectEnd()
      writer.writeObjectEnd()
      writer.writeObjectEnd()
    }
  )

  testCase(
    json = """[{"id":1,"value":"first"},{"id":2,"value":"second"}]""",
    iterate = writer => {
      writer.writeArrayStart()
      for ((id, value) <- List((1, "first"), (2, "second"))) {
        writer.writeObjectStart()
        writer.writeFieldName("id")
        writer.writeNumber(id)
        writer.writeFieldName("value")
        writer.writeString(value)
        writer.writeObjectEnd()
      }
      writer.writeArrayEnd()
    }
  )

  testCase(
    json =
      """{"data":{"users":[{"id":1,"name":"John","active":true,"scores":[10,20,30]},{"id":2,"name":"Jane","active":false,"scores":[15,25,35]}],"metadata":{"count":2,"version":"1.0"}}}""",
    iterate = writer => {
      writer.writeObjectStart()
      writer.writeFieldName("data")
      writer.writeObjectStart()

      writer.writeFieldName("users")
      writer.writeArrayStart()

      // First user
      writer.writeObjectStart()
      writer.writeFieldName("id")
      writer.writeNumber(1)
      writer.writeFieldName("name")
      writer.writeString("John")
      writer.writeFieldName("active")
      writer.writeBoolean(true)
      writer.writeFieldName("scores")
      writer.writeArrayStart()
      writer.writeNumber(10)
      writer.writeNumber(20)
      writer.writeNumber(30)
      writer.writeArrayEnd()
      writer.writeObjectEnd()

      writer.writeObjectStart()
      writer.writeFieldName("id")
      writer.writeNumber(2)
      writer.writeFieldName("name")
      writer.writeString("Jane")
      writer.writeFieldName("active")
      writer.writeBoolean(false)
      writer.writeFieldName("scores")
      writer.writeArrayStart()
      writer.writeNumber(15)
      writer.writeNumber(25)
      writer.writeNumber(35)
      writer.writeArrayEnd()
      writer.writeObjectEnd()

      writer.writeArrayEnd()

      writer.writeFieldName("metadata")
      writer.writeObjectStart()
      writer.writeFieldName("count")
      writer.writeNumber(2)
      writer.writeFieldName("version")
      writer.writeString("1.0")
      writer.writeObjectEnd()

      writer.writeObjectEnd()
      writer.writeObjectEnd()
    }
  )

  testCase(
    json = """{"raw":{"some":"json"}}""",
    iterate = writer => {
      writer.writeObjectStart()
      writer.writeFieldName("raw")
      writer.writeRawJson("""{"some":"json"}""")
      writer.writeObjectEnd()
    }
  )

  testCase(
    json = """""""",
    iterate = _.writeString("")
  )

  testCase(
    json =
      """{"empty_string":"","empty_array":[],"empty_object":{},"null_value":null}""",
    iterate = writer => {
      writer.writeObjectStart()
      writer.writeFieldName("empty_string")
      writer.writeString("")
      writer.writeFieldName("empty_array")
      writer.writeArrayStart().writeArrayEnd()
      writer.writeFieldName("empty_object")
      writer.writeObjectStart().writeObjectEnd()
      writer.writeFieldName("null_value")
      writer.writeNull()
      writer.writeObjectEnd()
    }
  )

}
