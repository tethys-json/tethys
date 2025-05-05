package tethys

import tethys.writers.tokens.TokenWriter

class OrdinalEnumJsonWriter[A <: scala.reflect.Enum] extends JsonWriter[A] {
  override def write(value: A, tokenWriter: TokenWriter): Unit =
    tokenWriter.writeNumber(value.ordinal)
}

object OrdinalEnumJsonWriter:
  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumJsonWriter[A] =
    new OrdinalEnumJsonWriter[A]

  private class WithLabel[A <: scala.reflect.Enum](label: String)
      extends JsonObjectWriter[A] {
    def writeValues(value: A, tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeFieldName(label)
      tokenWriter.writeNumber(value.ordinal)
    }
  }
  def withLabel[A <: scala.reflect.Enum](
      label: String
  ): JsonObjectWriter[A] =
    new WithLabel[A](label)
