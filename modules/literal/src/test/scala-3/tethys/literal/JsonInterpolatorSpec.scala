package tethys.literal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ListMap

import io.circe.Json

import tethys.*
import tethys.commons.RawJson
import tethys.commons.TokenNode.TokenNodesOps
import tethys.jackson.*
import tethys.literal.*
import tethys.writers.tokens.TokenWriterProducer

given explicitProducer: TokenWriterProducer = jacksonTokenWriterProducer

class JsonInterpolatorSpec extends AnyFlatSpec with Matchers {

  private def sameJson(
      actual: String,
      expected: String
  ): org.scalatest.Assertion =
    actual.jsonAsTokensList shouldBe expected.jsonAsTokensList

  private def sameJson(
      actual: RawJson,
      expected: String
  ): org.scalatest.Assertion =
    sameJson(actual.json, expected)

  private def sameJson(
      actual: RawJson,
      expected: Json
  ): org.scalatest.Assertion =
    sameJson(actual.json, expected.noSpaces)

  behavior of "json interpolator — primitives & null"

  it should "accept every JSON scalar form" in {
    sameJson(json"""5""", "5")
    sameJson(json"""-17""", "-17")
    sameJson(json"""0""", "0")
    sameJson(json"""3.14""", "3.14")
    sameJson(json"""1.5e10""", "1.5e10")
    sameJson(json"""-2.5E-3""", "-2.5E-3")
    sameJson(json"""6E+2""", "6E+2")
    sameJson(json"""true""", "true")
    sameJson(json"""false""", "false")
    sameJson(json"""null""", "null")
    sameJson(json""""hello"""", "\"hello\"")
    sameJson(json""""\u00A0\u1234"""", "\"\\u00A0\\u1234\"")
    sameJson(
      json""""tab\there\nnewline\\slash\/"""",
      "\"tab\\there\\nnewline\\\\slash\\/\""
    )
  }

  it should "accept empty composite literals" in {
    sameJson(json"""{}""", "{}")
    sameJson(json"""[]""", "[]")
    sameJson(json"""[[]]""", "[[]]")
    sameJson(json"""{"a": []}""", """{"a":[]}""")
  }

  behavior of "json interpolator — multiline literals"

  it should "accept a pretty-printed multiline object" in {
    val rj = json"""
      {
        "name": "Alice",
        "age":  30,
        "tags": ["scala", "json"],
        "addr": {
          "city": "Kazan",
          "zip":  "420000"
        }
      }
    """
    sameJson(
      rj,
      """{
        |  "name": "Alice",
        |  "age": 30,
        |  "tags": ["scala", "json"],
        |  "addr": {"city": "Kazan", "zip": "420000"}
        |}""".stripMargin
    )
  }

  it should "accept multiline arrays with comments-style spacing" in {
    val rj = json"""
      [
        1,
        2,

        3,
        [
          "x",
          "y"
        ]
      ]
    """
    sameJson(rj, """[1, 2, 3, ["x", "y"]]""")
  }

  it should "accept holes spread across multiple lines" in {
    val (name, age, city) = ("Bob", 25, "Innopolis")
    val rj = json"""
      {
        "name": $name,
        "age":  $age,
        "city": $city
      }
    """
    sameJson(rj, """{"name": "Bob", "age": 25, "city": "Innopolis"}""")
  }

  behavior of "json interpolator — value holes"

  it should "interpolate primitives with correct JSON encoding" in {
    val n: Int = 42
    val l: Long = 9000000000L
    val d: Double = 2.5
    val b: Boolean = true
    val s: String = "hi \"there\"\n\\slash"
    val none: Option[Int] = None
    val some: Option[Int] = Some(7)

    sameJson(
      json"""{
        "n": $n, "l": $l, "d": $d, "b": $b,
        "s": $s, "none": $none, "some": $some
      }""",
      """{
        |  "n": 42, "l": 9000000000, "d": 2.5, "b": true,
        |  "s": "hi \"there\"\n\\slash", "none": null, "some": 7
        |}""".stripMargin
    )
  }

  it should "interpolate collections in array and object positions" in {
    val xs = List(1, 2, 3)
    val nest = List(List(1, 2), List(3, 4, 5))
    val ss = Seq("a", "b\"c", "d\nE")

    sameJson(
      json"""{ "xs": $xs, "nest": $nest, "ss": $ss }""",
      """{ "xs": [1, 2, 3], "nest": [[1, 2], [3, 4, 5]], "ss": ["a", "b\"c", "d\nE"] }"""
    )
  }

  it should "interpolate Map[String, Int] preserving insertion order with ListMap" in {
    val m: Map[String, Int] = ListMap("a" -> 1, "b" -> 2, "c" -> 3)
    sameJson(json"""$m""", """{"a": 1, "b": 2, "c": 3}""")
  }

  case class User(name: String, age: Int) derives JsonObjectWriter
  case class Endpoint(path: String, methods: List[String])
      derives JsonObjectWriter

  it should "interpolate user case classes (single & inside structures)" in {
    val u = User("Alice", 30)
    val ep = Endpoint("/api/users", List("GET", "POST"))

    sameJson(json"""$u""", """{"name": "Alice", "age": 30}""")
    sameJson(
      json"""[1, $u, true]""",
      """[1, {"name": "Alice", "age": 30}, true]"""
    )
    sameJson(
      json"""{ "user": $u, "endpoint": $ep }""",
      """{
        |  "user": {"name": "Alice", "age": 30},
        |  "endpoint": {"path": "/api/users", "methods": ["GET", "POST"]}
        |}""".stripMargin
    )
  }

  it should "splice a RawJson value without re-quoting it" in {
    val inner = json"""{"inner": [1, 2]}"""
    sameJson(
      json"""{"outer": $inner, "tail": null}""",
      """{"outer": {"inner": [1, 2]}, "tail": null}"""
    )
  }

  behavior of "json interpolator — key holes"

  it should "interpolate keys of various types via KeyWriter" in {
    val ks = "dynamic"
    val ki = 7
    val ku = java.util.UUID.fromString("00000000-0000-0000-0000-000000000001")
    val kEs = "a\"b\nc"

    sameJson(json"""{$ks: 1}""", """{"dynamic": 1}""")
    sameJson(json"""{$ki: "v"}""", """{"7": "v"}""")
    sameJson(
      json"""{$ku: null}""",
      """{"00000000-0000-0000-0000-000000000001": null}"""
    )
    sameJson(json"""{$kEs: 1}""", """{"a\"b\nc": 1}""")
  }

  behavior of "json interpolator — heavy structural cases"

  case class Server(
      host: String,
      port: Int,
      ssl: Boolean,
      endpoints: List[Endpoint]
  ) derives JsonObjectWriter

  it should "match a deeply nested document built with circe AST, mixing literals, primitives, collections, Map and a case class hole" in {
    val host = "localhost"
    val port = 8080
    val timeout = 30
    val maxConn = 1000
    val features = List("auth", "logging")
    val getMethods = List("GET")
    val limits: Map[String, Int] =
      ListMap("maxConnections" -> maxConn, "timeout" -> timeout)
    val server = Server(
      host = host,
      port = port,
      ssl = true,
      endpoints = List(
        Endpoint("/api/users", List("GET", "POST")),
        Endpoint("/api/health", getMethods)
      )
    )

    val rj = json"""
      {
        "server": $server,
        "config": {
          "host": $host,
          "port": $port,
          "limits": $limits,
          "endpoints": [
            {
              "path": "/api/users",
              "methods": ["GET", "POST"],
              "headers": {
                "Content-Type": "application/json",
                "Accept": "application/json"
              }
            },
            {
              "path": "/api/health",
              "methods": $getMethods,
              "headers": { "Accept": "text/plain" }
            }
          ],
          "retries": null
        },
        "features": $features
      }
    """

    sameJson(
      rj,
      Json.obj(
        "server" -> Json.obj(
          "host" -> Json.fromString("localhost"),
          "port" -> Json.fromInt(8080),
          "ssl" -> Json.True,
          "endpoints" -> Json.arr(
            Json.obj(
              "path" -> Json.fromString("/api/users"),
              "methods" -> Json
                .arr(Json.fromString("GET"), Json.fromString("POST"))
            ),
            Json.obj(
              "path" -> Json.fromString("/api/health"),
              "methods" -> Json.arr(Json.fromString("GET"))
            )
          )
        ),
        "config" -> Json.obj(
          "host" -> Json.fromString("localhost"),
          "port" -> Json.fromInt(8080),
          "limits" -> Json.obj(
            "maxConnections" -> Json.fromInt(1000),
            "timeout" -> Json.fromInt(30)
          ),
          "endpoints" -> Json.arr(
            Json.obj(
              "path" -> Json.fromString("/api/users"),
              "methods" -> Json
                .arr(Json.fromString("GET"), Json.fromString("POST")),
              "headers" -> Json.obj(
                "Content-Type" -> Json.fromString("application/json"),
                "Accept" -> Json.fromString("application/json")
              )
            ),
            Json.obj(
              "path" -> Json.fromString("/api/health"),
              "methods" -> Json.arr(Json.fromString("GET")),
              "headers" -> Json.obj(
                "Accept" -> Json.fromString("text/plain")
              )
            )
          ),
          "retries" -> Json.Null
        ),
        "features" -> Json.arr(
          Json.fromString("auth"),
          Json.fromString("logging")
        )
      )
    )
  }

  it should "round-trip arbitrary string content through interpolation" in {
    val controls = (0 until 32).map(_.toChar).mkString
    val unicode = "привет, \uD83C\uDF0D и спецсимвол \u0301"
    val mixed = controls + "==" + unicode

    json"""$controls""".json.jsonAs[String] shouldBe Right(controls)
    json"""$unicode""".json.jsonAs[String] shouldBe Right(unicode)
    json"""$mixed""".json.jsonAs[String] shouldBe Right(mixed)
  }

  behavior of "json interpolator — compile-time validation"

  import scala.compiletime.testing.{typeCheckErrors, ErrorKind}

  inline val P =
    "import tethys.*; import tethys.jackson.*; import tethys.literal.*; "

  private inline def rejectsWith(
      inline code: String,
      inline expectedMessage: String
  ): org.scalatest.Assertion = {
    val errs = typeCheckErrors(code)
    val joined = errs.map(_.message).mkString("\n")
    withClue(s"diagnostics:\n$joined\n") {
      errs should not be empty
      joined.toLowerCase should include(expectedMessage.toLowerCase)
    }
  }

  it should "reject malformed structures with a precise parser message" in {
    rejectsWith(P + """json"42 oops"""", "unrecognized token")
    rejectsWith(P + """json"[1,]"""", "expected a valid value")
    rejectsWith(P + """json""""", "unexpected end")
    rejectsWith(P + """json"]"""", "unexpected close marker")
  }

  it should "reject malformed numbers with a precise parser message" in {
    rejectsWith(P + """json"-"""", "no digit following sign")
    rejectsWith(P + """json"01"""", "leading zeroes not allowed")
    rejectsWith(P + """json"1."""", "decimal point not followed by a digit")
    rejectsWith(P + """json"1e"""", "digit for number exponent")
  }

  it should "reject a value hole whose type has no JsonWriter and name the type" in {
    rejectsWith(
      P + """class NoWriter; val x = new NoWriter; json"${x}"""",
      "no jsonwriter[nowriter]"
    )
  }

  it should "reject a key hole whose type has no KeyWriter and name the type" in {
    rejectsWith(
      P + """class NoKey; val k = new NoKey; json"{${k}: 1}"""",
      "no keywriter[nokey]"
    )
  }

}
