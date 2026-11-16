package tethys.literal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import tethys.literal.JsonLiteralParser.{Hole, HolePosition, ParseError}

class JsonLiteralParserSpec extends AnyFlatSpec with Matchers {

  private def parse(parts: String*): Either[ParseError, List[Hole]] = {
    val partsList = parts.toList
    val badIdx = JsonLiteralParser.findHoleInString(partsList)
    if (badIdx >= 0)
      Left(
        ParseError(
          "Interpolation inside a string literal is not supported",
          badIdx
        )
      )
    else {
      val template = JsonLiteralParser.assembleTemplate(partsList)
      JsonLiteralParser.parse(template, parts.length - 1)
    }
  }

  private def ok(parts: String*)(expected: Hole*): Unit =
    parse(parts: _*) shouldBe Right(expected.toList)

  private def rejected(
      parts: String*
  )(messageSubstring: String): Unit = {
    val res = parse(parts: _*)
    res.isLeft shouldBe true
    val err = res.swap.toOption.get
    withClue(s"message was: ${err.message}") {
      err.message.toLowerCase should include(messageSubstring.toLowerCase)
    }
  }

  behavior of "JsonLiteralParser — primitives"

  it should "accept bare strings" in { ok("\"hello\"")() }
  it should "accept escaped strings" in {
    ok("\"a\\nb\\t\\\"c\\\\d\\/e\\b\\f\\r\"")()
  }
  it should "accept unicode escapes" in { ok("\"\\u00A0\\u1234\"")() }
  it should "accept integer" in { ok("42")() }
  it should "accept negative integer" in { ok("-17")() }
  it should "accept zero" in { ok("0")() }
  it should "accept decimal" in { ok("3.14")() }
  it should "accept scientific" in {
    ok("1.5e10")(); ok("-2.5E-3")(); ok("6E+2")()
  }
  it should "accept true/false/null" in {
    ok("true")(); ok("false")(); ok("null")()
  }

  behavior of "JsonLiteralParser — structural"

  it should "accept empty object" in { ok("{}")() }
  it should "accept empty array" in { ok("[]")() }
  it should "accept object with one field" in { ok("""{"a": 1}""")() }
  it should "accept nested object" in {
    ok("""{"a": {"b": {"c": {"d": 42}}}}""")()
  }
  it should "accept array of mixed types" in {
    ok("""[1, "s", true, null, 3.14, {"k": "v"}, []]""")()
  }
  it should "accept whitespace everywhere" in {
    ok("  \n\t  {  \"a\"  :  [ 1 , 2 ]  , \"b\" :  null  }  \n")()
  }

  behavior of "JsonLiteralParser — holes"

  it should "accept a single value hole" in {
    ok("", "")(Hole(0, HolePosition.Value))
  }
  it should "accept a value hole inside an object" in {
    ok("""{"a": """, "}")(Hole(0, HolePosition.Value))
  }
  it should "accept multiple value holes inside an array" in {
    ok("[", ", ", ", ", "]")(
      Hole(0, HolePosition.Value),
      Hole(1, HolePosition.Value),
      Hole(2, HolePosition.Value)
    )
  }
  it should "accept a key hole" in {
    ok("{", """: "v"}""")(Hole(0, HolePosition.Key))
  }
  it should "accept both key and value holes" in {
    ok("{", ": ", "}")(
      Hole(0, HolePosition.Key),
      Hole(1, HolePosition.Value)
    )
  }
  it should "accept deeply nested holes" in {
    ok("""{"a": [1, {"b": """, """}, 3], "c": """, "}")(
      Hole(0, HolePosition.Value),
      Hole(1, HolePosition.Value)
    )
  }

  behavior of "JsonLiteralParser — rejections"

  it should "reject trailing garbage" in {
    rejected("42 oops")("unrecognized token")
  }
  it should "reject unterminated string" in {
    rejected("\"abc")("expecting closing quote")
  }
  it should "reject invalid escape" in {
    rejected("\"\\q\"")("unrecognized character escape")
  }
  it should "reject truncated unicode escape" in {
    rejected("\"\\u12\"")("hex-digit")
  }
  it should "reject missing colon" in {
    rejected("""{"a" 1}""")("expecting a colon")
  }
  it should "reject missing comma" in {
    rejected("""{"a": 1 "b": 2}""")("expecting comma")
  }
  it should "reject dangling comma in object" in {
    rejected("""{"a": 1,}""")("expecting double-quote to start field name")
  }
  it should "reject dangling comma in array" in {
    rejected("[1,]")("expected a valid value")
  }
  it should "reject lone minus" in { rejected("-")("no digit following sign") }
  it should "reject number with leading zero" in {
    rejected("01")("leading zeroes not allowed")
  }
  it should "reject number with missing fraction digits" in {
    rejected("1.")("decimal point not followed by a digit")
  }
  it should "reject number with missing exponent digits" in {
    rejected("1e")("digit for number exponent")
  }
  it should "reject bare identifier" in {
    rejected("foo")("unrecognized token")
  }
  it should "reject hole inside a string" in {
    rejected("\"hello ", "\"")("inside a string literal is not supported")
  }
  it should "reject empty input" in { rejected("")("unexpected end") }
  it should "reject only whitespace" in {
    rejected("   \n  ")("unexpected end")
  }
  it should "reject closing without opening" in {
    rejected("]")("unexpected close marker")
  }
  it should "reject unbalanced braces" in {
    rejected("""{"a": 1""")("expected close marker")
  }
}
