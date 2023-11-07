package tethys.circe.ast

import scala.collection.mutable.ArrayBuffer
import io.circe.{Json, JsonNumber, JsonNumberHack, JsonObject}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.writers.tokens.TokenWriter

trait CirceSupport {
  implicit lazy val circeJsonObjectWriter: JsonWriter[JsonObject] =
    new JsonObjectWriter[JsonObject] {
      def writeValues(value: JsonObject, writer: TokenWriter): Unit = {
        val folder = new TethysJsonFolder(writer)
        val it = value.toIterable.iterator
        while (it.hasNext) {
          val (k, v) = it.next()

          writer.writeFieldName(k)
          v.foldWith(folder)
        }
      }
    }

  implicit lazy val circeJsonWriter: JsonWriter[Json] = new JsonWriter[Json] {
    def write(value: Json, writer: TokenWriter): Unit =
      value.foldWith(new TethysJsonFolder(writer))
  }

  implicit lazy val circeJsonObjectReader: JsonReader[JsonObject] =
    new JsonReader[JsonObject] {
      def read(it: TokenIterator)(implicit fieldName: FieldName): JsonObject =
        if (!it.currentToken().isObjectStart)
          ReaderError.wrongJson(
            s"Expected object start but found: ${it.currentToken()}"
          )
        else {
          it.next()

          var builder = ArrayBuffer.newBuilder[(String, Json)]
          while (!it.currentToken().isObjectEnd) {
            val token = it.currentToken()
            if (token.isFieldName) {
              val name = it.fieldName()
              val value =
                circeJsonReader.read(it.next())(fieldName.appendFieldName(name))

              builder += ((name, value))
            } else
              ReaderError.wrongJson(
                s"Expect end of object or field name but '$token' found"
              )(fieldName)
          }
          it.next()

          JsonObject.fromIterable(builder.result)
        }
    }

  implicit lazy val circeJsonReader: JsonReader[Json] = new JsonReader[Json] {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Json = {
      val token = it.currentToken()

      if (token.isObjectStart)
        Json.fromJsonObject(circeJsonObjectReader.read(it))
      else if (token.isArrayStart)
        Json.fromValues(JsonReader[Vector[Json]].read(it))
      else if (token.isStringValue)
        Json.fromString(JsonReader.stringReader.read(it))
      else if (token.isBooleanValue)
        Json.fromBoolean(JsonReader.booleanReader.read(it))
      else if (token.isNumberValue) JsonReader.numberReader.read(it) match {
        case x @ (_: java.lang.Byte | _: java.lang.Short | _: java.lang.Long) =>
          Json.fromLong(x.longValue)
        case x: java.lang.Integer => Json.fromInt(x)
        case x: java.lang.Float   => Json.fromFloatOrNull(x)
        case x: java.lang.Double  => Json.fromDoubleOrNull(x)

        case x: java.math.BigInteger => Json.fromBigInt(x)
        case x: BigInt               => Json.fromBigInt(x)

        case x: java.math.BigDecimal => Json.fromBigDecimal(x)
        case x: BigDecimal           => Json.fromBigDecimal(x)
        case x                       => Json.fromBigDecimal(x.doubleValue)
      }
      else if (token.isNullValue) { it.next(); Json.Null }
      else ReaderError.wrongJson(s"Unexpected token found: $token")(fieldName)
    }
  }

  private[this] class TethysJsonFolder(writer: TokenWriter)
      extends Json.Folder[Unit]
      with JsonNumberHack {
    def onNull: Unit = writer.writeNull()
    def onBoolean(value: Boolean): Unit = writer.writeBoolean(value)
    def onNumber(value: JsonNumber): Unit = writeNumber(value, writer)
    def onString(value: String): Unit = writer.writeString(value)
    def onArray(value: Vector[Json]): Unit = {
      writer.writeArrayStart()

      val it = value.iterator
      while (it.hasNext) it.next().foldWith(this)

      writer.writeArrayEnd()
    }
    def onObject(value: JsonObject): Unit = {
      writer.writeObjectStart()

      val it = value.toIterable.iterator
      while (it.hasNext) {
        val (k, v) = it.next()

        writer.writeFieldName(k)
        v.foldWith(this)
      }

      writer.writeObjectEnd()
    }
  }
}
