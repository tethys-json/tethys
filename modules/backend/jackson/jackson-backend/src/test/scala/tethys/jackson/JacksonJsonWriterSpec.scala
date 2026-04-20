package tethys.jackson

import tethys.writers.tokens.{
  TokenWriterProducer,
  TokenWriterConfig,
  JsonWriterSpec
}
import tethys._

class JacksonJsonWriterSpec extends JsonWriterSpec {
  {
    import tethys.jackson.pretty._
    import tethys.writers.tokens.TestModels._

    configurableTestCase(
      description = "pretty print complex structures",
      config = TokenWriterConfig.default.withDefaultPrettyPrinter,
      value = Department(
        "Global Engineering",
        List(
          Employee(
            1L,
            Person("John Doe", 30, Some("john@example.com")),
            Address("123 Main St", "New York", "USA", Some("10001")),
            "Backend",
            BigDecimal("100000.00")
          ),
          Employee(
            2L,
            Person("Jane Smith", 28, None),
            Address("456 Park Ave", "Boston", "USA", None),
            "Frontend",
            BigDecimal("95000.50")
          )
        ),
        Set("global", "engineering", "tech"),
        Map(
          "headquarters" -> "New York",
          "founded" -> "2020",
          "status" -> "active"
        )
      ),
      json = """{
  "name" : "Global Engineering",
  "employees" : [ {
    "id" : 1,
    "person" : {
      "name" : "John Doe",
      "age" : 30,
      "email" : "john@example.com"
    },
    "address" : {
      "street" : "123 Main St",
      "city" : "New York",
      "country" : "USA",
      "postalCode" : "10001"
    },
    "department" : "Backend",
    "salary" : 100000.00
  }, {
    "id" : 2,
    "person" : {
      "name" : "Jane Smith",
      "age" : 28
    },
    "address" : {
      "street" : "456 Park Ave",
      "city" : "Boston",
      "country" : "USA"
    },
    "department" : "Frontend",
    "salary" : 95000.50
  } ],
  "tags" : [ "global", "engineering", "tech" ],
  "metadata" : {
    "headquarters" : "New York",
    "founded" : "2020",
    "status" : "active"
  }
}"""
    )

    configurableTestCase(
      description = "escape unicode",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = "Unicode: \u00a9 \u20ac \u2603",
      json = "\"Unicode: \\u00A9 \\u20AC \\u2603\""
    )

    configurableTestCase(
      description = "escape unicode emoji",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = "Emoji: 😀 🚀 🌍",
      json = "\"Emoji: \\uD83D\\uDE00 \\uD83D\\uDE80 \\uD83C\\uDF0D\""
    )

    configurableTestCase(
      description = "unicode characters in object keys",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = Map("café" -> "coffee", "résumé" -> "CV", "über" -> "super"),
      json =
        "{\"caf\\u00E9\":\"coffee\",\"r\\u00E9sum\\u00E9\":\"CV\",\"\\u00FCber\":\"super\"}"
    )

    case class MenuItem(name: String, price: Double)
    case class Menu(items: List[MenuItem])
    case class Restaurant(menu: Menu)

    implicit val menuItemWriter: JsonWriter[MenuItem] = JsonWriter
      .obj[MenuItem]
      .addField("name")(_.name)
      .addField("price")(_.price)

    implicit val menuWriter: JsonWriter[Menu] = JsonWriter
      .obj[Menu]
      .addField("items")(_.items)

    implicit val restaurantWriter: JsonWriter[Restaurant] = JsonWriter
      .obj[Restaurant]
      .addField("menu")(_.menu)

    configurableTestCase(
      description = "unicode characters in nested objects",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = Restaurant(
        Menu(
          List(
            MenuItem("Café au lait", 3.50),
            MenuItem("Crème brûlée", 5.75)
          )
        )
      ),
      json =
        "{\"menu\":{\"items\":[{\"name\":\"Caf\\u00E9 au lait\",\"price\":3.5},{\"name\":\"Cr\\u00E8me br\\u00FBl\\u00E9e\",\"price\":5.75}]}}"
    )

    configurableTestCase(
      description = "mixed ASCII and unicode in arrays",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = List("hello", "世界", "こんにちは", "안녕하세요"),
      json =
        "[\"hello\",\"\\u4E16\\u754C\",\"\\u3053\\u3093\\u306B\\u3061\\u306F\",\"\\uC548\\uB155\\uD558\\uC138\\uC694\"]"
    )

    configurableTestCase(
      description = "control characters that should always be escaped",
      config = TokenWriterConfig.default.withEscapeUnicode(false),
      value = "Control chars: \u0000\u0001\u0008\u0009\u000A\u000C\u000D\u001F",
      json = "\"Control chars: \\u0000\\u0001\\b\\t\\n\\f\\r\\u001F\""
    )

    configurableTestCase(
      description = "unicode characters with escapeUnicode set to false",
      config = TokenWriterConfig.default.withEscapeUnicode(false),
      value = "Unicode: \u00a9 \u20ac \u2603",
      json = "\"Unicode: © € ☃\""
    )

    case class SurrogatePairsTest(
        text: String,
        list: List[String],
        keyValue: (String, String)
    )

    implicit val surrogatePairsTestWriter: JsonWriter[SurrogatePairsTest] =
      JsonWriter
        .obj[SurrogatePairsTest]
        .addField("text")(_.text)
        .addField("list")(_.list)
        .addField("nested")(test => Map(test.keyValue))

    configurableTestCase(
      description = "surrogate pairs in different contexts",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = SurrogatePairsTest(
        "Surrogate pairs: 𝄞 𝌆 𝓐",
        List("𝄞", "𝌆", "𝓐"),
        ("key𝄞", "value𝌆")
      ),
      json =
        "{\"text\":\"Surrogate pairs: \\uD834\\uDD1E \\uD834\\uDF06 \\uD835\\uDCD0\",\"list\":[\"\\uD834\\uDD1E\",\"\\uD834\\uDF06\",\"\\uD835\\uDCD0\"],\"nested\":{\"key\\uD834\\uDD1E\":\"value\\uD834\\uDF06\"}}"
    )
  }

}
