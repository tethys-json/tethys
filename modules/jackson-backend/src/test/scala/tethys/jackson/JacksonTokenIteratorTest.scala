package tethys.jackson

import tethys._
import tethys.commons.TokenNode._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec

class JacksonTokenIteratorTest extends AnyFlatSpec with Matchers {

  behavior of "JacksonTokenIterator"

  it should "properly iterate over json string" in {
    val json = """{"a":1,"b":["s",true,{"a":null},1.0,false]}"""
    val it = json.toTokenIterator.fold(throw _, identity)
    it.currentToken().isObjectStart shouldBe true

    it.nextToken().isFieldName shouldBe true
    it.fieldName() shouldBe "a"
    it.nextToken().isNumberValue shouldBe true
    it.number() shouldBe 1

    it.nextToken().isFieldName shouldBe true
    it.fieldName() shouldBe "b"
    it.nextToken().isArrayStart shouldBe true

    it.nextToken().isStringValue shouldBe true
    it.string() shouldBe "s"

    it.nextToken().isBooleanValue shouldBe true
    it.boolean() shouldBe true

    it.nextToken().isObjectStart shouldBe true
    it.nextToken().isFieldName shouldBe true
    it.fieldName() shouldBe "a"
    it.nextToken().isNullValue shouldBe true
    it.nextToken().isObjectEnd shouldBe true

    it.nextToken().isNumberValue shouldBe true
    it.number() shouldBe 1.0

    it.nextToken().isBooleanValue shouldBe true
    it.boolean() shouldBe false

    it.nextToken().isArrayEnd shouldBe true
    it.nextToken().isObjectEnd shouldBe true

    it.nextToken().isEmpty shouldBe true
  }

  it should "correctly skip next expressions" in {
    val json = """{"a":1,"b":["s",true,{"a":null},1.0,false]}"""
    val it = json.toTokenIterator.fold(throw _, identity)
    it.currentToken().isObjectStart shouldBe true

    it.nextToken().isFieldName shouldBe true
    it.fieldName() shouldBe "a"
    it.next().skipExpression()

    it.currentToken().isFieldName shouldBe true
    it.fieldName() shouldBe "b"
    it.next().skipExpression()
    it.currentToken().isObjectEnd shouldBe true

    it.nextToken().isEmpty shouldBe true
  }

  it should "correctly collect expressions" in {
    val json = """{"a":1,"b":["s",true,{"a":null},1.0,false]}"""
    val it = json.toTokenIterator.fold(throw _, identity)
    it.currentToken().isObjectStart shouldBe true

    it.nextToken().isFieldName shouldBe true
    it.fieldName() shouldBe "a"

    val a = it.next().collectExpression()
    a.currentToken().isNumberValue shouldBe true
    a.number() shouldBe 1
    a.nextToken().isEmpty shouldBe true

    it.currentToken().isFieldName shouldBe true
    it.fieldName() shouldBe "b"

    val b = it.next().collectExpression()
    b.currentToken().isArrayStart shouldBe true

    b.nextToken().isStringValue shouldBe true
    b.string() shouldBe "s"

    b.nextToken().isBooleanValue shouldBe true
    b.boolean() shouldBe true

    b.nextToken().isObjectStart shouldBe true
    b.nextToken().isFieldName shouldBe true
    b.fieldName() shouldBe "a"
    b.nextToken().isNullValue shouldBe true
    b.nextToken().isObjectEnd shouldBe true

    b.nextToken().isNumberValue shouldBe true
    b.number() shouldBe 1.0

    b.nextToken().isBooleanValue shouldBe true
    b.boolean() shouldBe false

    b.nextToken().isArrayEnd shouldBe true
    b.nextToken().isEmpty shouldBe true


    it.currentToken().isObjectEnd shouldBe true

    it.nextToken().isEmpty shouldBe true

  }

  it should "generate proper tokens seq" in {
    val json = """{"a":1,"b":["s",true,{"a":null},1.0,false]}"""

    json.jsonAsTokensList shouldBe obj(
      "a" -> 1,
      "b" -> arr(
        "s",
        true,
        obj(
          "a" -> null
        ),
        1.0,
        false
      )
    )
  }
}
