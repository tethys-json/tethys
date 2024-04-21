package tethys
import tethys.writers.tokens.TokenWriter

trait StringEnumWriter[A] extends JsonWriter[A]

object StringEnumWriter:
  inline def derived[A <: scala.reflect.Enum]: StringEnumWriter[A] = 
    (value: A, tokenWriter: TokenWriter) => tokenWriter.writeString(value.toString)

  inline def withLabel[A <: scala.reflect.Enum](label: String): JsonObjectWriter[A] =
    (value: A, tokenWriter: writers.tokens.TokenWriter) =>
      tokenWriter.writeFieldName(label)
      tokenWriter.writeString(value.toString)

