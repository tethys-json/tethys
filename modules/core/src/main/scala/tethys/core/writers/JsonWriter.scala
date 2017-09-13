package tethys.core.writers

import com.fasterxml.jackson.core.JsonGenerator
import tethys.core.commons.LowPriorityInstance
import tethys.core.writers.instances.{BasicWriters, ComplexWriters}

import scala.language.higherKinds

trait JsonWriter[A] {

  def write(name: String, value: A, jsonGenerator: JsonGenerator): Unit = {
    jsonGenerator.writeFieldName(name)
    write(value, jsonGenerator)
  }

  def write(value: A, jsonGenerator: JsonGenerator): Unit
}

object JsonWriter
  extends BasicWriters
    with ComplexWriters
    with LowPriorityJsonWriters {
  def apply[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[A] = jsonWriter
}

private[writers] trait LowPriorityJsonWriters {
  implicit final def lowPriorityWriter[A](implicit lowPriorityInstance: LowPriorityInstance[JsonWriter[A]]): JsonWriter[A] = {
    lowPriorityInstance.instance
  }
}
