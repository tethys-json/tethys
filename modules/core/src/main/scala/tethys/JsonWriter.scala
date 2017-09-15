package tethys

import tethys.commons.LowPriorityInstance
import tethys.writers.instances.{BasicWriters, ComplexWriters}
import tethys.writers.tokens.TokenWriter

import scala.language.higherKinds

trait JsonWriter[A] {

  def write(name: String, value: A, tokenWriter: TokenWriter): Unit = {
    tokenWriter.writeFieldName(name)
    write(value, tokenWriter)
  }

  def write(value: A, tokenWriter: TokenWriter): Unit
}

object JsonWriter
  extends BasicWriters
    with ComplexWriters
    with LowPriorityJsonWriters {
  def apply[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[A] = jsonWriter
}

private[tethys] trait LowPriorityJsonWriters {
  implicit final def lowPriorityWriter[A](implicit lowPriorityInstance: LowPriorityInstance[JsonWriter[A]]): JsonWriter[A] = {
    lowPriorityInstance.instance
  }
}
