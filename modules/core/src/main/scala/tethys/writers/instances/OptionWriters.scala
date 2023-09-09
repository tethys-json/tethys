package tethys.writers.instances

import tethys.JsonWriter
import tethys.writers.tokens.TokenWriter

private[tethys] trait OptionWriters extends MapWriters {
  implicit lazy val noneWriter: JsonWriter[None.type] =
    new JsonWriter[None.type] {
      override def write(
          name: String,
          value: None.type,
          tokenWriter: TokenWriter
      ): Unit = ()
      override def write(value: None.type, tokenWriter: TokenWriter): Unit =
        tokenWriter.writeNull()
    }

  implicit def someWriter[A](implicit
      jsonWriter: JsonWriter[A]
  ): JsonWriter[Some[A]] = new JsonWriter[Some[A]] {
    override def write(value: Some[A], tokenWriter: TokenWriter): Unit = {
      jsonWriter.write(value.get, tokenWriter)
    }
  }

  implicit def optionalWriter[A](implicit
      valueWriter: JsonWriter[A]
  ): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {

    override def write(
        name: String,
        value: Option[A],
        tokenWriter: TokenWriter
    ): Unit = {
      if (value.nonEmpty) {
        valueWriter.write(name, value.get, tokenWriter)
      }
    }

    override def write(value: Option[A], tokenWriter: TokenWriter): Unit = {
      if (value.isEmpty) tokenWriter.writeNull()
      else valueWriter.write(value.get, tokenWriter)
    }
  }
}
