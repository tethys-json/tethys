package tethys.readers.tokens

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys._

trait TokenIteratorSpec extends AnyFlatSpec with Matchers {

  def producer: TokenIteratorProducer

  behavior of "TokenIterator"

  def testCase(
      json: String,
      assertions: TokenIterator => Unit
  ): Unit = {
    it should s"iterate over $json" in {
      val iter = json.toTokenIterator(producer).fold(throw _, identity)
      assertions(iter)
    }
  }

  testCase(
    json = """true""",
    assertions = iterator => {
      iterator.currentToken().isBooleanValue shouldBe true
      iterator.boolean() shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """false""",
    assertions = iterator => {
      iterator.currentToken().isBooleanValue shouldBe true
      iterator.boolean() shouldBe false
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """"foo-bar"""",
    assertions = iterator => {
      iterator.currentToken().isStringValue shouldBe true
      iterator.string() shouldBe "foo-bar"
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """["special \"quotes\""]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isStringValue shouldBe true
      iterator.string() shouldBe """special "quotes""""
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """123""",
    assertions = iterator => {
      iterator.currentToken().isNumberValue shouldBe true
      iterator.int() shouldBe 123
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """-123""",
    assertions = iterator => {
      iterator.currentToken().isNumberValue shouldBe true
      iterator.int() shouldBe -123
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """-123.5""",
    assertions = iterator => {
      iterator.currentToken().isNumberValue shouldBe true
      iterator.double() shouldBe -123.5
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """123.5""",
    assertions = iterator => {
      iterator.currentToken().isNumberValue shouldBe true
      iterator.double() shouldBe 123.5
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """null""",
    assertions = iterator => {
      iterator.currentToken().isNullValue shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[null]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isNullValue shouldBe true
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[true]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isBooleanValue shouldBe true
      iterator.boolean() shouldBe true
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[123]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isNumberValue shouldBe true
      iterator.number().longValue() shouldBe 123
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """["test"]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isStringValue shouldBe true
      iterator.string() shouldBe "test"
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[1, 2]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isNumberValue shouldBe true
      iterator.int() shouldBe 1
      iterator.nextToken().isNumberValue shouldBe true
      iterator.int() shouldBe 2
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[1, "text", 123.5, true, null]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true

      iterator.nextToken().isNumberValue shouldBe true
      iterator.number().intValue() shouldBe 1

      iterator.nextToken().isStringValue shouldBe true
      iterator.string() shouldBe "text"

      iterator.nextToken().isNumberValue shouldBe true
      iterator.number().doubleValue() shouldBe 123.5

      iterator.nextToken().isBooleanValue shouldBe true
      iterator.boolean() shouldBe true

      iterator.nextToken().isNullValue shouldBe true

      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[[1, 2]]""",
    assertions = iterator => {
      iterator.currentToken().isArrayStart shouldBe true
      iterator.nextToken().isArrayStart shouldBe true

      iterator.nextToken().isNumberValue shouldBe true
      iterator.number().intValue() shouldBe 1

      iterator.nextToken().isNumberValue shouldBe true
      iterator.number().intValue() shouldBe 2

      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isArrayEnd shouldBe true
      iterator.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = "{}",
    assertions = reader => {
      reader.currentToken().isObjectStart shouldBe true
      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """{"name": "test"}""",
    assertions = reader => {
      reader.currentToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "name"

      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "test"

      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json =
      """{"id": 123, "name": "test", "active": true, "score": 98.6, "tags": null}""",
    assertions = reader => {
      reader.currentToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "id"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 123

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "name"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "test"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "active"
      reader.nextToken().isBooleanValue shouldBe true
      reader.boolean() shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "score"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().doubleValue() shouldBe 98.6

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "tags"
      reader.nextToken().isNullValue shouldBe true

      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """{"user": {"id": 1, "profile": {"name": "John", "age": 30}}}""",
    assertions = reader => {
      reader.currentToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "user"

      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "id"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 1

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "profile"

      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "name"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "John"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "age"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 30

      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[{"id": 1, "value": "first"}, {"id": 2, "value": "second"}]""",
    assertions = reader => {
      reader.currentToken().isArrayStart shouldBe true

      reader.nextToken().isObjectStart shouldBe true
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "id"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 1
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "value"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "first"
      reader.nextToken().isObjectEnd shouldBe true

      reader.nextToken().isObjectStart shouldBe true
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "id"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 2
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "value"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "second"
      reader.nextToken().isObjectEnd shouldBe true

      reader.nextToken().isArrayEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """{
          "id": 1,
          "metadata": {
            "created": "2023-01-01",
            "tags": ["important", "urgent"]
          },
          "data": {
            "items": [
              {
                "name": "item1",
                "properties": {
                  "color": "red",
                  "size": 10,
                  "features": [true, false, null]
                }
              },
              {
                "name": "item2",
                "properties": {
                  "color": "blue",
                  "size": 20,
                  "features": [false, true, null]
                }
              }
            ],
            "summary": {
              "total": 2,
              "active": true
            }
          }
        }""",
    assertions = reader => {
      reader.currentToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "id"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 1

      // Metadata object
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "metadata"
      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "created"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "2023-01-01"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "tags"
      reader.nextToken().isArrayStart shouldBe true
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "important"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "urgent"
      reader.nextToken().isArrayEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true

      // Data object
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "data"
      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "items"
      reader.nextToken().isArrayStart shouldBe true

      // First item
      reader.nextToken().isObjectStart shouldBe true
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "name"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "item1"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "properties"
      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "color"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "red"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "size"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 10

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "features"
      reader.nextToken().isArrayStart shouldBe true
      reader.nextToken().isBooleanValue shouldBe true
      reader.boolean() shouldBe true
      reader.nextToken().isBooleanValue shouldBe true
      reader.boolean() shouldBe false
      reader.nextToken().isNullValue shouldBe true
      reader.nextToken().isArrayEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true

      // Second item
      reader.nextToken().isObjectStart shouldBe true
      reader.skipExpression()
      reader.currentToken().isArrayEnd shouldBe true
      // Summary object

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "summary"
      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "total"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 2

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "active"
      reader.nextToken().isBooleanValue shouldBe true
      reader.boolean() shouldBe true

      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isObjectEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

  testCase(
    json = """[
      {
        "string": "first",
        "int": 1,
        "boolean": true,
        "bigDecimal": 11.11,
        "seqInt": [1, 2],
        "mapStringInt": {"a": 1, "b": 2}
      },
      {
        "string": "second",
        "int": 2,
        "boolean": false,
        "bigDecimal": 22.22,
        "seqInt": [3, 4],
        "mapStringInt": {"c": 3, "d": 4}
      }
    ]""",
    assertions = reader => {
      reader.currentToken().isArrayStart shouldBe true

      // First Data object
      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "string"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "first"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "int"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 1

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "boolean"
      reader.nextToken().isBooleanValue shouldBe true
      reader.boolean() shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "bigDecimal"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().doubleValue() shouldBe 11.11

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "seqInt"
      reader.nextToken().isArrayStart shouldBe true
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 1
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 2
      reader.nextToken().isArrayEnd shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "mapStringInt"
      reader.nextToken().isObjectStart shouldBe true
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "a"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 1
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "b"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 2
      reader.nextToken().isObjectEnd shouldBe true

      reader.nextToken().isObjectEnd shouldBe true

      // Second Data object
      reader.nextToken().isObjectStart shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "string"
      reader.nextToken().isStringValue shouldBe true
      reader.string() shouldBe "second"

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "int"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 2

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "boolean"
      reader.nextToken().isBooleanValue shouldBe true
      reader.boolean() shouldBe false

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "bigDecimal"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().doubleValue() shouldBe 22.22

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "seqInt"
      reader.nextToken().isArrayStart shouldBe true
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 3
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 4
      reader.nextToken().isArrayEnd shouldBe true

      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "mapStringInt"
      reader.nextToken().isObjectStart shouldBe true
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "c"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 3
      reader.nextToken().isFieldName shouldBe true
      reader.fieldName() shouldBe "d"
      reader.nextToken().isNumberValue shouldBe true
      reader.number().intValue() shouldBe 4
      reader.nextToken().isObjectEnd shouldBe true

      reader.nextToken().isObjectEnd shouldBe true

      reader.nextToken().isArrayEnd shouldBe true
      reader.nextToken().isEmpty shouldBe true
    }
  )

//  testCase(
//    json = """["escaped\\backslash"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator.string() shouldBe """escaped\backslash"""
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  testCase(
//    json = """["unicode\u0020char"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator.string() shouldBe "unicode char"
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  testCase(
//    json = """["multi
//  line
//  string"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator.string() shouldBe "multi\nline\nstring"
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  testCase(
//    json = """["special chars: \b\f\n\r\t"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator.string() shouldBe "special chars: \b\f\n\r\t"
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  testCase(
//    json = """["\u0001\u001F control chars"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator.string() shouldBe "\u0001\u001F control chars"
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  testCase(
//    json = """["emoji: 🚀"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator.string() shouldBe "emoji: 🚀"
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  testCase(
//    json = """["mixed: special \"quote\" and unicode \u0020 and emoji 🎉"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      iterator
//        .string() shouldBe """mixed: special "quote" and unicode   and emoji 🎉"""
//      iterator.nextToken().isArrayEnd shouldBe true
//      iterator.nextToken().isEmpty shouldBe true
//    }
//  )
//
//  // Error cases
//  testCase(
//    json = """["unclosed string""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      assertThrows[RuntimeException] {
//        iterator.string()
//      }
//    }
//  )
//
//  testCase(
//    json = """["invalid escape \z"]""",
//    assertions = iterator => {
//      iterator.currentToken().isArrayStart shouldBe true
//      iterator.nextToken().isStringValue shouldBe true
//      assertThrows[RuntimeException] {
//        iterator.string()
//      }
//    }
//  )
//

}
