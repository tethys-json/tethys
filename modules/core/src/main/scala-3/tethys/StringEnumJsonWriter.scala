package tethys
import tethys.writers.tokens.TokenWriter

trait StringEnumJsonWriter[A] extends JsonWriter[A]

object StringEnumJsonWriter:
  inline def derived[A <: scala.reflect.Enum]: StringEnumJsonWriter[A] =
    (value: A, tokenWriter: TokenWriter) => tokenWriter.writeString(value.toString)

  inline def withLabel[A <: scala.reflect.Enum](label: String): JsonObjectWriter[A] =
    (value: A, tokenWriter: writers.tokens.TokenWriter) =>
      tokenWriter.writeFieldName(label)
      tokenWriter.writeString(value.toString)

