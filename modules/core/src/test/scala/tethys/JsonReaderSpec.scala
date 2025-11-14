package tethys

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.readers.ReaderError
import tethys.readers.tokens.TokenIteratorProducer

trait JsonReaderSpec extends AnyFlatSpec with Matchers {
  behavior of "JsonReader"

  implicit def producer: TokenIteratorProducer

  def testCase[A: JsonReader](
      json: String,
      expected: Either[String, A]
  )(implicit pos: org.scalactic.source.Position): Unit =
    it should s"read $json" in {
      json.jsonAs[A].left.map(_.getMessage) shouldBe expected
    }

  testCase[Int]("1", Right(1))
  testCase[Int]("-1", Right(-1))
  testCase[Double]("1.5", Right(1.5))
  testCase[Double]("-1.5", Right(-1.5))
  testCase[String](
    "",
    Left("Illegal json at '[ROOT]': Expected string value but found: Empty")
  )
  testCase[String](""" "" """, Right(""))
  testCase[String](""" "1.5" """, Right("1.5"))

  testCase[Boolean]("true", Right(true))
  testCase[Boolean]("false", Right(false))
  testCase[Boolean](
    "\"true\"",
    Left(
      "Illegal json at '[ROOT]': Expected boolean value but found: StringValueToken"
    )
  )

  testCase[Long]("9223372036854775807", Right(9223372036854775807L))
  testCase[Long]("-9223372036854775808", Right(-9223372036854775808L))
  testCase[BigDecimal]("123.456789", Right(BigDecimal("123.456789")))
  testCase[Float]("3.14", Right(3.14f))
  testCase[Byte]("127", Right(127.toByte))
  testCase[Short]("32767", Right(32767.toShort))

  testCase[List[Int]]("[1,2,3]", Right(List(1, 2, 3)))
  testCase[List[Int]]("[]", Right(List.empty))
  testCase[Vector[String]]("""["a","b","c"]""", Right(Vector("a", "b", "c")))
  testCase[Set[Int]]("[1,2,2,3]", Right(Set(1, 2, 3)))
  testCase[Seq[Double]]("[1.1, 2.2, 3.3]", Right(Seq(1.1, 2.2, 3.3)))

  testCase[Map[String, Int]](
    """{"a":1,"b":2}""",
    Right(Map("a" -> 1, "b" -> 2))
  )

  testCase[Option[Int]]("null", Right(None))
  testCase[Option[Int]]("42", Right(Some(42)))
  testCase[Option[String]]("\"test\"", Right(Some("test")))
  testCase[Option[List[Int]]]("[3,4,5]", Right(Some(List(3, 4, 5))))

  testCase[Map[String, List[Option[Int]]]](
    """{"a":[1,null,3],"b":[]}""",
    Right(Map("a" -> List(Some(1), None, Some(3)), "b" -> List()))
  )

  testCase[List[Map[String, String]]](
    """[{"k1":"v1"},{"k2":"v2"}]""",
    Right(List(Map("k1" -> "v1"), Map("k2" -> "v2")))
  )

  testCase[Int](
    "\"not_a_number\"",
    Left(
      "Illegal json at '[ROOT]': Expected int value but found: StringValueToken"
    )
  )

  testCase[List[Int]](
    "[1,\"not_a_number\",3]",
    Left(
      "Illegal json at '[ROOT][1]': Expected int value but found: StringValueToken"
    )
  )

  testCase[Map[String, Int]](
    """{"a":"not_a_number"}""",
    Left(
      "Illegal json at '[ROOT].a': Expected int value but found: StringValueToken"
    )
  )

  /* //TODO unify error messages

  testCase[Int](
    "9223372036854775808",
    Left(
      "Numeric value (9223372036854775808) out of range of int (-2147483648 - 2147483647)\n at [Source: (String)\"9223372036854775808\"; line: 1, column: 20]"
    )
  )*/

  testCase[String](
    "\"\\\"escaped_quotes\\\"\"",
    Right("\"escaped_quotes\"")
  )

  testCase[String](
    "\"special\\nchars\\t\\r\"",
    Right("special\nchars\t\r")
  )

  testCase[String](
    "\"unicode\\u0026char\"",
    Right("unicode&char")
  )

  testCase[List[Int]](
    "   [  1  ,  2  ,  3  ]   ",
    Right(List(1, 2, 3))
  )

  testCase[Map[String, String]](
    """  {  "key"  :  "value"  }  """,
    Right(Map("key" -> "value"))
  )

  testCase[Map[String, List[Map[String, Option[Int]]]]](
    """
      {
        "first": [
          {"a": 1, "b": null},
          {"c": 3}
        ],
        "second": []
      }
      """,
    Right(
      Map(
        "first" -> List(
          Map("a" -> Some(1), "b" -> None),
          Map("c" -> Some(3))
        ),
        "second" -> List()
      )
    )
  )

  testCase[Double]("1.23e-4", Right(1.23e-4))
  testCase[Double]("1.23E+4", Right(1.23e+4))
  testCase[BigDecimal]("1.23e-5", Right(BigDecimal("1.23e-5")))

  testCase[String](
    """[1,"text",true,null,{"k":"v"}]""",
    Left(
      "Illegal json at '[ROOT]': Expected string value but found: ArrayStartToken"
    )
  )
  /* TODO unify error messages
  testCase[Map[String, Int]](
    "{a:1}",
    Left(
      "Unexpected character ('a' (code 97)): was expecting double-quote to start field name\n at [Source: (String)\"{a:1}\"; line: 1, column: 3]"
    )
  )*/

  /* //TODO unify error messages
 testCase[List[Int]](
    "[1,2,3",
    Left(
      "Unexpected end-of-input: expected close marker for Array (start marker at [Source: (String)\"[1,2,3\"; line: 1, column: 1])\n at [Source: (String)\"[1,2,3\"; line: 1, column: 7]"
    )
  )*/

  /* //TODO unify error messages
  testCase[Map[String, String]](
    """{"key": "value" "key2": "value2"}""",
    Left(
      "Unexpected character ('\"' (code 34)): was expecting comma to separate Object entries\n at [Source: (String)\"{\"key\": \"value\" \"key2\": \"value2\"}\"; line: 1, column: 18]"
    )
  )*/

  case class Data(
      string: String,
      int: Int,
      boolean: Boolean,
      bigDecimal: BigDecimal,
      seqInt: Seq[Int],
      mapStringInt: Map[String, Int]
  )

  implicit val dataReader: JsonReader[Data] = JsonReader.builder
    .addField[String]("string")
    .addField[Int]("int")
    .addField[Boolean]("boolean")
    .addField[BigDecimal]("bigDecimal")
    .addField[Seq[Int]]("seqInt")
    .addField[Map[String, Int]]("mapStringInt")
    .buildReader(Data(_, _, _, _, _, _))

  testCase[Data](
    """{
      "string": "first",
      "int": 1,
      "boolean": true,
      "bigDecimal": 11.11,
      "seqInt": [1, 2],
      "mapStringInt": {"a": 1, "b": 2}
    }""",
    Right(
      Data(
        string = "first",
        int = 1,
        boolean = true,
        bigDecimal = BigDecimal(11.11),
        seqInt = List(1, 2),
        mapStringInt = Map("a" -> 1, "b" -> 2)
      )
    )
  )

}
