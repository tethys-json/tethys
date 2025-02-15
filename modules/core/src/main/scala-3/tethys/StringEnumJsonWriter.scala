package tethys
import tethys.writers.tokens.TokenWriter

class StringEnumJsonWriter[A <: scala.reflect.Enum] extends JsonWriter[A] {
  def write(value: A, tokenWriter: TokenWriter): Unit =
    tokenWriter.writeString(value.toString)
}

object StringEnumJsonWriter:
  inline def derived[A <: scala.reflect.Enum]: StringEnumJsonWriter[A] =
    new StringEnumJsonWriter[A]

  private class WithLabel[A <: scala.reflect.Enum](label: String)
      extends JsonObjectWriter[A] {
    def writeValues(value: A, tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeFieldName(label)
      tokenWriter.writeString(value.toString)
    }
  }

  def withLabel[A <: scala.reflect.Enum](
      label: String
  ): JsonObjectWriter[A] = new WithLabel[A](label)
