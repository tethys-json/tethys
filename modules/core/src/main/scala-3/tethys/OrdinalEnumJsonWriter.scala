package tethys
import tethys.writers.tokens.TokenWriter


trait OrdinalEnumJsonWriter[A] extends JsonWriter[A]

object OrdinalEnumJsonWriter:
  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumJsonWriter[A] =
    (value: A, tokenWriter: TokenWriter) => tokenWriter.writeNumber(value.ordinal)

  inline def withLabel[A <: scala.reflect.Enum](label: String): JsonObjectWriter[A] =
    (value: A, tokenWriter: writers.tokens.TokenWriter) =>
      tokenWriter.writeFieldName(label)
      tokenWriter.writeNumber(value.ordinal)


