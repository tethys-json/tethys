package tethys.jackson

import tethys.core.readers._
import org.scalatest.{FlatSpec, Matchers}

class JacksonTokenIteratorTest extends FlatSpec with Matchers {
  "JacksonTokenIterator" should "properly iterate over json string" in {
    val json = """{"a":1,"b":["s",true,{"a":null},1.0,false]}"""
    val it = json.toTokenIterator
    it.currentToken().isObjectStart shouldBe true

    it.nextToken().isFieldName shouldBe true
    it.fieldName() should contain("a")
    it.nextToken().isNumberValue shouldBe true
    it.number() should contain(1)

    it.nextToken().isFieldName shouldBe true
    it.fieldName() should contain("b")
    it.nextToken().isArrayStart shouldBe true

    it.nextToken().isStringValue shouldBe true
    it.string() should contain("s")

    it.nextToken().isBooleanValue shouldBe true
    it.boolean() should contain(true)

    it.nextToken().isObjectStart shouldBe true
    it.nextToken().isFieldName shouldBe true
    it.fieldName() should contain("a")
    it.nextToken().isNullValue shouldBe true
    it.nextToken().isObjectEnd shouldBe true

    it.nextToken().isNumberValue shouldBe true
    it.number() should contain(1.0)

    it.nextToken().isBooleanValue shouldBe true
    it.boolean() should contain(false)

    it.nextToken().isArrayEnd shouldBe true
    it.nextToken().isObjectEnd shouldBe true

    it.nextToken().isEmpty shouldBe true

  }
}
