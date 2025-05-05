package tethys.writers.tokens

import tethys._

class DefaultJsonWriterSpec extends JsonWriterSpec {
  {
    import TestModels._

    configurableTestCase(
      description = "unicode characters in object keys",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = Map("café" -> "coffee", "résumé" -> "CV", "über" -> "super"),
      json = "{\"caf\\u00e9\":\"coffee\",\"r\\u00e9sum\\u00e9\":\"CV\",\"\\u00fcber\":\"super\"}"
    )

    case class MenuItem(name: String, price: Double)
    case class Menu(items: List[MenuItem])
    case class Restaurant(menu: Menu)

    implicit val menuItemWriter: JsonWriter[MenuItem] = JsonWriter.obj[MenuItem]
      .addField("name")(_.name)
      .addField("price")(_.price)

    implicit val menuWriter: JsonWriter[Menu] = JsonWriter.obj[Menu]
      .addField("items")(_.items)

    implicit val restaurantWriter: JsonWriter[Restaurant] = JsonWriter.obj[Restaurant]
      .addField("menu")(_.menu)

    configurableTestCase(
      description = "unicode characters in nested objects",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = Restaurant(Menu(List(
        MenuItem("Café au lait", 3.50),
        MenuItem("Crème brûlée", 5.75)
      ))),
      json = "{\"menu\":{\"items\":[{\"name\":\"Caf\\u00e9 au lait\",\"price\":3.5},{\"name\":\"Cr\\u00e8me br\\u00fbl\\u00e9e\",\"price\":5.75}]}}"
    )

    configurableTestCase(
      description = "mixed ASCII and unicode in arrays",
      config = TokenWriterConfig.default.withEscapeUnicode(true),
      value = List("hello", "世界", "こんにちは", "안녕하세요"),
      json = "[\"hello\",\"\\u4e16\\u754c\",\"\\u3053\\u3093\\u306b\\u3061\\u306f\",\"\\uc548\\ub155\\ud558\\uc138\\uc694\"]"
    )

    configurableTestCase(
      description = "control characters that should always be escaped",
      config = TokenWriterConfig.default.withEscapeUnicode(false),
      value = "Control chars: \u0000\u0001\u0008\u0009\u000A\u000C\u000D\u001F",
      json = "\"Control chars: \\u0000\\u0001\\b\\t\\n\\f\\r\\u001f\""
    )

    configurableTestCase(
      description = "unicode characters with escapeUnicode set to false",
      config = TokenWriterConfig.default.withEscapeUnicode(false),
      value = "Unicode: \u00a9 \u20ac \u2603",
      json = "\"Unicode: © € ☃\""
    )

    case class SurrogatePairsTest(text: String, list: List[String], keyValue: (String, String))

    implicit val surrogatePairsTestWriter: JsonWriter[SurrogatePairsTest] = JsonWriter.obj[SurrogatePairsTest]
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
      json = "{\"text\":\"Surrogate pairs: \\ud834\\udd1e \\ud834\\udf06 \\ud835\\udcd0\",\"list\":[\"\\ud834\\udd1e\",\"\\ud834\\udf06\",\"\\ud835\\udcd0\"],\"nested\":{\"key\\ud834\\udd1e\":\"value\\ud834\\udf06\"}}"
    )
  }
}