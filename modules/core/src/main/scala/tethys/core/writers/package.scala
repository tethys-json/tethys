package tethys.core

import java.io.StringWriter

import tethys.core.writers.tokens.{TokenWriter, TokenWriterProducer}

package object writers {

  implicit class JsonWriterOps[A](val a: A) extends AnyVal {
    def asJson(implicit jsonWriter: JsonWriter[A], tokenWriterProducer: TokenWriterProducer): String = {
      val stringWriter = new StringWriter()
      writeJson(tokenWriterProducer.forWriter(stringWriter))
      stringWriter.toString
    }

    def writeJson(tokenWriter: TokenWriter)(implicit jsonWriter: JsonWriter[A]): Unit = {
      try jsonWriter.write(a, tokenWriter) finally {
        tokenWriter.close()
      }
    }
  }
}
