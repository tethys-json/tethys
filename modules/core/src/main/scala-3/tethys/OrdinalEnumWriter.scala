package tethys
import tethys.writers.tokens.TokenWriter


trait OrdinalEnumWriter[A] extends JsonWriter[A]

object OrdinalEnumWriter:
  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumWriter[A] =
    (value: A, tokenWriter: TokenWriter) => tokenWriter.writeNumber(value.ordinal)

  inline def withLabel[A <: scala.reflect.Enum](label: String): JsonObjectWriter[A] =
    (value: A, tokenWriter: writers.tokens.TokenWriter) =>
      tokenWriter.writeFieldName(label)
      tokenWriter.writeNumber(value.ordinal)


