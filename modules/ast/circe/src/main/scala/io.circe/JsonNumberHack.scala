package io.circe

import tethys.writers.tokens.TokenWriter

trait JsonNumberHack {
  protected def writeNumber(number: JsonNumber, writer: TokenWriter): Unit = {
    if (JsonNumberHack.isHackCompatible) {
      number match {
        case value: JsonDecimal =>
          writer.writeRawJson(value.toString)
        case value: JsonBiggerDecimal =>
          writer.writeRawJson(value.toString)
        case value: JsonBigDecimal =>
          writer.writeNumber(value.value)
        case value: JsonLong =>
          writer.writeNumber(value.value)
        case value: JsonDouble =>
          writer.writeNumber(value.value)
        case value: JsonFloat =>
          writer.writeNumber(value.value)
      }
    } else {
      import io.circe.syntax._
      writer.writeRawJson(number.asJson.noSpaces)
    }
  }
}

object JsonNumberHack {
  private val isHackCompatible: Boolean =
    try {
      val loader = getClass.getClassLoader
      loader.loadClass("io.circe.BiggerDecimalJsonNumber")
      loader.loadClass("io.circe.JsonDecimal")
      loader.loadClass("io.circe.JsonBiggerDecimal")
      loader.loadClass("io.circe.JsonBigDecimal")
      loader.loadClass("io.circe.JsonLong")
      loader.loadClass("io.circe.JsonDouble")
      loader.loadClass("io.circe.JsonFloat")
      true
    } catch {
      case _: ClassNotFoundException => false
    }
}
