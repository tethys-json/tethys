package tethys.core.writers
import com.fasterxml.jackson.core.JsonGenerator

trait EmptyWriters {
  def emptyWriter[A]: JsonWriter[A] = new JsonWriter[A] {
    override def write(name: String, value: A, jsonGenerator: JsonGenerator): Unit = ()
    override def write(value: A, jsonGenerator: JsonGenerator): Unit = ()
  }
}

object EmptyWriters extends EmptyWriters
