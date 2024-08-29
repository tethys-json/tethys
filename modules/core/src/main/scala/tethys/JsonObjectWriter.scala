package tethys

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

  def concat(that: JsonObjectWriter[A]): JsonObjectWriter[A] =
    new JsonObjectWriter[A] {
      override def writeValues(value: A, tokenWriter: TokenWriter): Unit = {
        self.writeValues(value, tokenWriter)
        that.writeValues(value, tokenWriter)
      }
    }

  override def contramap[B](fun: B => A): JsonObjectWriter[B] =
    new JsonObjectWriter[B] {
      override def writeValues(value: B, tokenWriter: TokenWriter): Unit =
        self.writeValues(fun(value), tokenWriter)
    }
}

object JsonObjectWriter
    extends LowPriorityJsonObjectWriters
    with derivation.JsonObjectWriterDerivation {
  def apply[A](implicit
      jsonObjectWriter: JsonObjectWriter[A]
  ): JsonObjectWriter[A] = jsonObjectWriter
}

private[tethys] trait LowPriorityJsonObjectWriters {
  implicit final def lowPriorityWriter[A](implicit
      lowPriorityInstance: LowPriorityInstance[JsonObjectWriter[A]]
  ): JsonObjectWriter[A] = {
    lowPriorityInstance.instance
  }
}
