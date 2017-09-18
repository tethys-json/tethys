package tethys.writers

import tethys.JsonWriter
import tethys.commons.LowPriorityInstance
import tethys.writers.tokens.TokenWriter

trait JsonObjectWriter[A] extends JsonWriter[A] {
  self =>

  override def write(value: A, tokenWriter: TokenWriter): Unit = {
    tokenWriter.writeObjectStart()
    writeValues(value, tokenWriter)
    tokenWriter.writeObjectEnd()
  }

  def writeValues(value: A, tokenWriter: TokenWriter): Unit

  def ++(that: JsonObjectWriter[A]): JsonObjectWriter[A] = concat(that)

  def concat(that: JsonObjectWriter[A]): JsonObjectWriter[A] = new JsonObjectWriter[A] {
    override def writeValues(value: A, tokenWriter: TokenWriter): Unit = {
      self.writeValues(value, tokenWriter)
      that.writeValues(value, tokenWriter)
    }
  }
}

object JsonObjectWriter extends LowPriorityJsonObjectWriters {
  def apply[A](implicit jsonWriter: JsonObjectWriter[A]): JsonObjectWriter[A] = jsonWriter
}

private[tethys] trait LowPriorityJsonObjectWriters {
  implicit final def lowPriorityObjectWriter[A](implicit lowPriorityInstance: LowPriorityInstance[JsonObjectWriter[A]]): JsonObjectWriter[A] = {
    lowPriorityInstance.instance
  }
}
