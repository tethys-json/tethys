package tethys.writers.instances

import tethys.{JsonObjectWriter, JsonWriter}
import tethys.writers.instances.SimpleJsonObjectWriter.JsonFieldObjectField
import tethys.writers.tokens.TokenWriter

import scala.collection.immutable

class SimpleJsonObjectWriter[A](val fields: Seq[JsonFieldObjectField[A, _]])
    extends JsonObjectWriter[A] {

  override def writeValues(value: A, tokenWriter: TokenWriter): Unit = {
    val it = fields.iterator
    while (it.hasNext) {
      it.next() match {
        case JsonFieldObjectField(name, fun, jsonWriter) =>
          jsonWriter.write(name, fun.apply(value), tokenWriter)
      }
    }
  }

  def addField[B](name: String)(
      fun: A => B
  )(implicit jsonWriter: JsonWriter[B]): SimpleJsonObjectWriter[A] = {
    SimpleJsonObjectWriter(
      fields :+ JsonFieldObjectField[A, B](name, fun, jsonWriter)
    )
  }

  def ++(that: SimpleJsonObjectWriter[A]): SimpleJsonObjectWriter[A] = concat(
    that
  )

  def concat(that: SimpleJsonObjectWriter[A]): SimpleJsonObjectWriter[A] =
    SimpleJsonObjectWriter(this.fields ++ that.fields)
}

object SimpleJsonObjectWriter {

  def apply[A]: SimpleJsonObjectWriter[A] =
    new SimpleJsonObjectWriter[A](immutable.Queue.empty)

  def apply[A](
      fields: Seq[JsonFieldObjectField[A, _]]
  ): SimpleJsonObjectWriter[A] = new SimpleJsonObjectWriter[A](fields)

  case class JsonFieldObjectField[A, B](
      name: String,
      fun: A => B,
      jsonWriter: JsonWriter[B]
  )
}
