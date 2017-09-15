package tethys.jackson

import java.io.StringWriter

import org.scalatest.{FlatSpec, Matchers}
import tethys._
import tethys.writers.tokens.TokenWriter

class JacksonTokenWriterTest extends FlatSpec with Matchers {

  def iterate(fun: (TokenWriter) => Unit): String = {
    val sw = new StringWriter()
    val tokenWriter = sw.toTokenWriter
    fun(tokenWriter)
    tokenWriter.close()
    sw.toString
  }

  behavior of "JacksonTokenWriter"

  it should "write String value" in {
    iterate(_.writeString("string")) shouldBe """"string""""
  }

  it should "write Short value" in {
    iterate(_.writeNumber(1: Short)) shouldBe """1"""
  }

  it should "write Int value" in {
    iterate(_.writeNumber(1: Int)) shouldBe """1"""
  }

  it should "write Long value" in {
    iterate(_.writeNumber(1: Long)) shouldBe """1"""
  }

  it should "write Float value" in {
    iterate(_.writeNumber(1: Float)) shouldBe """1.0"""
  }

  it should "write Double value" in {
    iterate(_.writeNumber(1: Double)) shouldBe """1.0"""
  }

  it should "write BitInt value" in {
    iterate(_.writeNumber(1: BigInt)) shouldBe """1"""
  }

  it should "write BigDecimal value" in {
    iterate(_.writeNumber(1: BigDecimal)) shouldBe """1"""
  }

  it should "write true value" in {
    iterate(_.writeBoolean(true)) shouldBe """true"""
  }

  it should "write false value" in {
    iterate(_.writeBoolean(false)) shouldBe """false"""
  }

  it should "write null value" in {
    iterate(_.writeNull()) shouldBe """null"""
  }

  it should "write object structure" in {
    iterate(_.writeObjectStart().writeObjectEnd()) shouldBe """{}"""
  }

  it should "write array structure" in {
    iterate(_.writeArrayStart().writeArrayEnd()) shouldBe """[]"""
  }

  it should "write complex object structure" in {
    iterate {
      _.writeObjectStart()
        .writeFieldName("a")
        .writeNumber(1)
        .writeFieldName("b")
        .writeArrayStart()
        .writeString("s")
        .writeBoolean(true)
        .writeObjectStart()
        .writeFieldName("a")
        .writeNull()
        .writeObjectEnd()
        .writeArrayEnd()
        .writeObjectEnd()
    } shouldBe """{"a":1,"b":["s",true,{"a":null}]}"""
  }

}
