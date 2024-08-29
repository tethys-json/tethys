package tethys.json4s

import org.json4s.JsonAST._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.commons.TokenNode.{value => token, _}
import tethys.writers.tokens.SimpleTokenWriter._

class Json4sSupportTest extends AnyFlatSpec with Matchers {
  behavior of "Json4s ast JsonReader"

  it should "parse JLong" in {
    token(100L).tokensAs[JLong] shouldBe JLong(100L)
  }

  it should "parse JInt" in {
    token(100L).tokensAs[JInt] shouldBe JInt(100)
  }

  it should "parse JDouble" in {
    token(100.0d).tokensAs[JDouble] shouldBe JDouble(100.0d)
  }

  it should "parse JDecimal" in {
    token(100.0d).tokensAs[JDecimal] shouldBe JDecimal(100.0d)
  }

  it should "parse JString" in {
    token("str").tokensAs[JString] shouldBe JString("str")
  }

  it should "parse JBool.True" in {
    token(true).tokensAs[JBool] shouldBe JBool.True
  }

  it should "parse JBool.False" in {
    token(false).tokensAs[JBool] shouldBe JBool.False
  }

  it should "parse JArray" in {
    arr(1, 2L, 3).tokensAs[JArray] shouldBe JArray(
      List(JLong(1), JLong(2), JLong(3))
    )
  }

  it should "parse JArray of JObject" in {
    arr(obj("a" -> "b", "c" -> "d")).tokensAs[JValue] shouldBe JArray(
      List(JObject("a" -> JString("b"), "c" -> JString("d")))
    )
  }

  it should "parse JSet" in {
    arr(1, 2L, 3).tokensAs[JSet] shouldBe JSet(
      Set(JLong(1), JLong(2), JLong(3))
    )
  }

  it should "parse JObject" in {
    obj("a" -> arr(1), "b" -> obj("c" -> null))
      .tokensAs[JObject] shouldBe JObject(
      "a" -> JArray(List(JLong(1L))),
      "b" -> JObject("c" -> JNull)
    )
  }

  behavior of "Json4s ast JsonWriter"
  it should "write JLong" in {
    JLong(100L).asTokenList shouldBe token(100L)
  }

  it should "write JInt" in {
    JInt(100).asTokenList shouldBe token(BigInt(100L))
  }

  it should "write JDouble" in {
    JDouble(100.0d).asTokenList shouldBe token(100.0d)
  }

  it should "write JDecimal" in {
    JDecimal(100.0d).asTokenList shouldBe token(BigDecimal(100.0d))
  }

  it should "write JString" in {
    JString("str").asTokenList shouldBe token("str")
  }

  it should "write JBool.True" in {
    JBool.True.asTokenList shouldBe token(true)
  }

  it should "write JBool.False" in {
    JBool.False.asTokenList shouldBe token(false)
  }

  it should "write JArray" in {
    JArray(List(JLong(1), JLong(2), JLong(3))).asTokenList shouldBe arr(
      1L,
      2L,
      3L
    )
  }

  it should "write JSet" in {
    JSet(Set(JLong(1), JLong(2), JLong(3))).asTokenList shouldBe arr(1L, 2L, 3L)
  }

  it should "write JObject" in {
    val jobj = JObject(
      "a" -> JArray(List(JLong(1L))),
      "b" -> JObject("c" -> JNull)
    )
    jobj.asTokenList shouldBe obj(
      "a" -> arr(1L),
      "b" -> obj("c" -> null)
    )
  }
}
