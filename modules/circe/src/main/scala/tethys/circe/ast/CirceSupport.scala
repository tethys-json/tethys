package tethys.circe.ast

import io.circe.{Json, JsonObject}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.writers.tokens.TokenWriter

trait CirceSupport {
  implicit lazy val circeJsonObjectWriter: JsonWriter[JsonObject] = new JsonObjectWriter[JsonObject] {
    def writeValues(value: JsonObject, writer: TokenWriter): Unit =
      value.toIterable.foreach { case (k, v) => circeJsonWriter.write(k, v, writer) }
  }

  implicit lazy val circeJsonWriter: JsonWriter[Json] = new JsonWriter[Json] {
    def write(value: Json, writer: TokenWriter): Unit = value.fold(
      writer.writeNull(),
      bool   => writer.writeBoolean(bool),
      // TODO Maybe we can do better? WDYT
      number => number.getClass.getName match {
        case "io.circe.JsonLong" =>
          writer.writeNumber(number.toLong.get)
        case "io.circe.JsonDouble" | "io.circe.JsonFloat" =>
          writer.writeNumber(number.toDouble)
        case _ =>
          writer.writeNumber(number.toBigDecimal.get)
      },
      str    => writer.writeString(str),
      array  => JsonWriter[Vector[Json]].write(array, writer),
      obj    => circeJsonObjectWriter.write(obj, writer)
    )
  }

  implicit lazy val circeJsonObjectReader: JsonReader[JsonObject] = new JsonReader[JsonObject] {
    def read(it: TokenIterator)(implicit fieldName: FieldName): JsonObject = {
      if (!it.currentToken().isObjectStart) ReaderError.wrongJson(s"Expected object start but found: ${it.currentToken()}")
      else {
        it.next()
        var builder = JsonObject.empty
        while (!it.currentToken().isObjectEnd) {
          val token = it.currentToken()
          if (token.isFieldName) {
            val name = it.fieldName()
            val value = circeJsonReader.read(it.next())(fieldName.appendFieldName(name))
            builder = builder.add(name, value)
          } else {
            ReaderError.wrongJson(s"Expect end of object or field name but '$token' found")(fieldName)
          }
        }
        it.next()
        builder
      }
    }
  }

  implicit lazy val circeJsonReader: JsonReader[Json] = new JsonReader[Json] {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Json = {
      val token = it.currentToken()

      if (token.isObjectStart) Json.fromJsonObject(circeJsonObjectReader.read(it))
      else if (token.isArrayStart) Json.fromValues(JsonReader[Vector[Json]].read(it))
      else if (token.isStringValue) Json.fromString(JsonReader[String].read(it))
      else if (token.isBooleanValue) Json.fromBoolean(JsonReader[Boolean].read(it))
      else if (token.isNumberValue) JsonReader[Number].read(it) match {
        case x@(_: java.lang.Short | _: java.lang.Integer | _: java.lang.Long) => Json.fromLong(x.longValue())
        case x@(_: java.lang.Float | _: java.lang.Double)                      => Json.fromDoubleOrNull(x.doubleValue())

        case x: java.math.BigInteger => Json.fromBigInt(x)
        case x: BigInt               => Json.fromBigInt(x)

        case x: java.math.BigDecimal => Json.fromBigDecimal(x)
        case x: BigDecimal           => Json.fromBigDecimal(x)
        case x                       => Json.fromBigDecimal(x.doubleValue())
      }
      else if (token.isNullValue) { it.next(); Json.Null }
      else ReaderError.wrongJson(s"Unexpected token found: $token")(fieldName)
    }
  }
}
