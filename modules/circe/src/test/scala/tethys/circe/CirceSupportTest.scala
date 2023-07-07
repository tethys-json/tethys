package tethys.circe

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import io.circe.{Json, JsonObject}
import tethys.commons.TokenNode
import tethys.commons.TokenNode.{value => token, _}
import tethys.circe.SimpleTokenWriterRaw._

class CirceSupportTest extends AnyFlatSpec with Matchers {
  behavior of "Circe ast JsonReader"

  it should "parse Int" in {
    token(100L).tokensAs[Json] shouldBe Json.fromInt(100)
  }

  it should "parse Long" in {
    token(100L).tokensAs[Json] shouldBe Json.fromLong(100L)
  }

  it should "parse Float" in {
    token(100.0f).tokensAs[Json] shouldBe Json.fromFloatOrNull(100.0f)
  }

  it should "parse Double" in {
    token(100.0d).tokensAs[Json] shouldBe Json.fromDoubleOrNull(100.0d)
  }

  it should "parse BigInt" in {
    token(BigInt(100L)).tokensAs[Json] shouldBe Json.fromBigInt(BigInt(100L))
  }

  it should "parse BigDecimal" in {
    token(BigDecimal(100.0d)).tokensAs[Json] shouldBe Json.fromBigDecimal(
      100.0d
    )
  }

  it should "parse String" in {
    token("str").tokensAs[Json] shouldBe Json.fromString("str")
  }

  it should "parse Boolean.True" in {
    token(true).tokensAs[Json] shouldBe Json.True
  }

  it should "parse Boolean.False" in {
    token(false).tokensAs[Json] shouldBe Json.False
  }

  it should "parse Null" in {
    List(TokenNode.NullValueNode).tokensAs[Json] shouldBe Json.Null
  }

  it should "parse Array" in {
    arr(1, 2L, 3).tokensAs[Json] shouldBe
      Json.fromValues(
        List(Json.fromLong(1L), Json.fromLong(2L), Json.fromLong(3L))
      )
  }

  it should "parse JsonObject" in {
    obj(
      "a" -> arr(1L, 2L),
      "b" -> obj("c" -> null),
      "c" -> token("demo"),
      "d" -> token(true),
      "e" -> token(false)
    ).tokensAs[JsonObject] shouldBe JsonObject(
      "a" -> Json.fromValues(List(Json.fromLong(1L), Json.fromLong(2L))),
      "b" -> Json.fromJsonObject(JsonObject("c" -> Json.Null)),
      "c" -> Json.fromString("demo"),
      "d" -> Json.True,
      "e" -> Json.False
    )
  }

  it should "parse Array of JsonObject" in {
    arr(obj("a" -> "b"), obj("c" -> "d"))
      .tokensAs[Json] shouldBe Json.fromValues(
      List(
        Json.fromJsonObject(JsonObject("a" -> Json.fromString("b"))),
        Json.fromJsonObject(JsonObject("c" -> Json.fromString("d")))
      )
    )
  }

  behavior of "Circe ast JsonWriter"

  it should "write Int" in {
    Json.fromInt(100).asTokenList shouldBe token(100L)
  }

  it should "write Long" in {
    Json.fromLong(10000000000L).asTokenList shouldBe token(10000000000L)
  }

  it should "write Float" in {
    Json.fromFloat(100.0f).asTokenList shouldBe token(100.0f)
  }

  it should "write Double" in {
    Json.fromDouble(100.0d).asTokenList shouldBe token(100.0d)
  }

  it should "write BigInt" in {
    Json.fromBigInt(BigInt("10000000000")).asTokenList match {
      case DoubleValueNode(d) :: Nil => d shouldBe 1.0e10 // 2.11 only behavior
      case LongValueNode(l) :: Nil   => l shouldBe 10000000000L
      case _                         => fail()
    }
  }

  it should "write BigDecimal" in {
    Json.fromBigDecimal(BigDecimal(100.0d)).asTokenList shouldBe token(
      BigDecimal(100.0d)
    )
  }

  it should "write String" in {
    Json.fromString("str").asTokenList shouldBe token("str")
  }

  it should "write Boolean.True" in {
    Json.fromBoolean(true).asTokenList shouldBe token(true)
  }

  it should "write Boolean.False" in {
    Json.fromBoolean(false).asTokenList shouldBe token(false)
  }

  it should "write Null" in {
    Json.Null.asTokenList shouldBe List(TokenNode.NullValueNode)
  }

  it should "write Array" in {
    Json
      .fromValues(
        List(
          Json.fromInt(1),
          Json.fromInt(2),
          Json.fromInt(3)
        )
      )
      .asTokenList shouldBe arr(1L, 2L, 3L)
  }

  it should "write JsonObject" in {
    val jobj = JsonObject(
      "a" -> Json.fromValues(List(Json.fromInt(1), Json.fromInt(2))),
      "b" -> Json.fromJsonObject(JsonObject("c" -> Json.Null)),
      "c" -> Json.fromString("demo"),
      "d" -> Json.True,
      "e" -> Json.False
    )

    jobj.asTokenList shouldBe obj(
      "a" -> arr(1L, 2L),
      "b" -> obj("c" -> null),
      "c" -> token("demo"),
      "d" -> token(true),
      "e" -> token(false)
    )
  }
}
