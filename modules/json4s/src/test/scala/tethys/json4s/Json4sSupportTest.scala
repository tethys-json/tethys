package tethys.json4s

import org.json4s.JsonAST._
import org.scalatest.{FlatSpec, Matchers}
import tethys.commons.TokenNode.{value => token, _}

class Json4sSupportTest extends FlatSpec with Matchers {
  behavior of "Json4s ast JsonReader"

  it should "parse JLong" in {
    token(100L).tokensAs[JLong] shouldBe JLong(100L)
  }

  it should "parse JInt" in {
    token(100L).tokensAs[JInt] shouldBe JInt(100)
  }

  it should "parse JDouble" in {
    token(100.0D).tokensAs[JDouble] shouldBe JDouble(100.0D)
  }

  it should "parse JDecimal" in {
    token(100.0D).tokensAs[JDecimal] shouldBe JDecimal(100.0D)
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
    arr(1, 2, 3).tokensAs[JArray] shouldBe JArray(List(JLong(1), JLong(2), JLong(3)))
  }

  it should "parse JSet" in {
    arr(1, 2, 3).tokensAs[JSet] shouldBe JSet(Set(JLong(1), JLong(2), JLong(3)))
  }

  it should "parse JObject" in {
    obj("a" -> arr(1), "b" -> obj("c" -> null)).tokensAs[JObject] shouldBe JObject(
      "a" -> JArray(List(JLong(1L))),
      "b" -> JObject("c" -> JNull)
    )
  }

}
