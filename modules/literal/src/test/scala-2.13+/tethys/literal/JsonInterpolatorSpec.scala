package tethys.literal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

import tethys._
import tethys.commons.RawJson
import tethys.commons.TokenNode.TokenNodesOps
import tethys.derivation.semiauto._
import tethys.jackson._
import tethys.literal.JsonInterpolator._

class JsonInterpolatorSpec extends AnyFlatSpec with Matchers {

  private def sameJson(actual: String, expected: String): org.scalatest.Assertion =
    actual.jsonAsTokensList shouldBe expected.jsonAsTokensList

  private def sameJson(actual: RawJson, expected: String): org.scalatest.Assertion =
    sameJson(actual.json, expected)

  behavior of "json interpolator on Scala 2"

  it should "accept primitive literals" in {
    sameJson(json"""5""", "5")
    sameJson(json"""-3.14""", "-3.14")
    sameJson(json"""true""", "true")
    sameJson(json"""null""", "null")
    sameJson(json""""hello"""", "\"hello\"")
    sameJson(json"""{}""", "{}")
    sameJson(json"""[]""", "[]")
  }

  it should "accept a multiline literal with no holes" in {
    val rj = json"""
      {
        "a": 1,
        "b": [1, 2, 3],
        "c": null
      }
    """
    sameJson(rj, """{"a": 1, "b": [1, 2, 3], "c": null}""")
  }

  it should "interpolate primitives" in {
    val n = 42
    val s = "hi \"there\"\n\\slash"
    sameJson(json"""{"n": $n, "s": $s}""", """{"n": 42, "s": "hi \"there\"\n\\slash"}""")
  }

  it should "interpolate collections and Map[String, Int] with deterministic order" in {
    val xs                       = List(1, 2, 3)
    val m: Map[String, Int]      = ListMap("a" -> 1, "b" -> 2)
    sameJson(json"""$xs""", "[1, 2, 3]")
    sameJson(json"""$m""", """{"a": 1, "b": 2}""")
  }

  case class User(name: String, age: Int)
  object User {
    implicit val jw: JsonObjectWriter[User] = jsonWriter[User]
  }

  it should "interpolate a case class with a derived JsonObjectWriter" in {
    val u = User("Alice", 30)
    sameJson(json"""$u""", """{"name": "Alice", "age": 30}""")
    sameJson(json"""[1, $u, true]""", """[1, {"name": "Alice", "age": 30}, true]""")
  }

  it should "interpolate keys via KeyWriter" in {
    val ks = "dynamic"
    val ki = 7
    sameJson(json"""{$ks: 1}""", """{"dynamic": 1}""")
    sameJson(json"""{$ki: "v"}""", """{"7": "v"}""")
  }

  it should "splice a RawJson value without re-quoting it" in {
    val inner = json"""{"inner": [1, 2]}"""
    sameJson(json"""{"outer": $inner}""", """{"outer": {"inner": [1, 2]}}""")
  }

  it should "round-trip arbitrary string content" in {
    val controls = (0 until 32).map(_.toChar).mkString
    val unicode  = "привет, мир \u0301"
    json"""$controls""".json.jsonAs[String] shouldBe Right(controls)
    json"""$unicode""".json.jsonAs[String] shouldBe Right(unicode)
  }

  it should "assemble a deeply nested document mixing literals, primitives, collections and a case class hole" in {
    val u                       = User("Alice", 30)
    val m: Map[String, Int]     = ListMap("maxConn" -> 100, "timeout" -> 30)
    val tags                    = List("scala", "json")
    val rj = json"""
      {
        "user": $u,
        "tags": $tags,
        "limits": $m,
        "meta": { "active": true, "retries": null }
      }
    """
    sameJson(
      rj,
      """{
        |  "user": {"name": "Alice", "age": 30},
        |  "tags": ["scala", "json"],
        |  "limits": {"maxConn": 100, "timeout": 30},
        |  "meta": {"active": true, "retries": null}
        |}""".stripMargin
    )
  }
}
