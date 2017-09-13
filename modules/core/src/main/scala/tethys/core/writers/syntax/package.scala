package tethys.core.writers

import java.io.StringWriter

import com.fasterxml.jackson.core.JsonFactory

package object syntax {
  protected lazy val defaultJsonFactory: JsonFactory = new JsonFactory()

  def writeString[A](value: A, jsonFactory: JsonFactory = defaultJsonFactory)(implicit jsonWriter: JsonWriter[A]): String = {
    val writer = new StringWriter()
    val generator = jsonFactory.createGenerator(writer)
    try jsonWriter.write(value, generator) finally {
      generator.close()
    }

    writer.toString
  }

  def writePrettyString[A](value: A, jsonFactory: JsonFactory = defaultJsonFactory)(implicit jsonWriter: JsonWriter[A]): String = {
    val writer = new StringWriter()
    val generator = defaultJsonFactory.createGenerator(writer).useDefaultPrettyPrinter()
    try jsonWriter.write(value, generator) finally {
      generator.close()
    }

    writer.toString
  }

  implicit class JsonWriterOps[A](val a: A) extends AnyVal {
    def asJson(implicit jsonWriter: JsonWriter[A]): String = writeString[A](a)
    def asJson(jsonFactory: JsonFactory)(implicit jsonWriter: JsonWriter[A]): String = writeString[A](a, jsonFactory)

    def asPrettyJson(implicit jsonWriter: JsonWriter[A]): String = writePrettyString[A](a)
    def asPrettyJson(jsonFactory: JsonFactory)(implicit jsonWriter: JsonWriter[A]): String = writePrettyString[A](a, jsonFactory)
  }
}
