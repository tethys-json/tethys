import java.io.{Reader, StringReader, StringWriter, Writer}

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{TokenWriter, TokenWriterProducer}

package object tethys {

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

  implicit class WriterOps(val w: Writer) extends AnyVal {
    def toTokenWriter(implicit tokenWriterProducer: TokenWriterProducer): TokenWriter = tokenWriterProducer.forWriter(w)
  }

  implicit class StringReaderOps(val json: String) extends AnyVal {
    def jsonAs[A](implicit jsonReader: JsonReader[A], producer: TokenIteratorProducer): Either[ReaderError, A] = {
      new StringReader(json).readJson[A]
    }

    def toTokenIterator(implicit producer: TokenIteratorProducer): TokenIterator = {
      new StringReader(json).toTokenIterator
    }
  }

  implicit class ReaderReaderOps(val reader: Reader) extends AnyVal {
    def readJson[A](implicit jsonReader: JsonReader[A], producer: TokenIteratorProducer): Either[ReaderError, A] = {
      producer.fromReader(reader).readJson[A]
    }

    def toTokenIterator(implicit producer: TokenIteratorProducer): TokenIterator = {
      producer.fromReader(reader)
    }
  }

  implicit class TokenIteratorOps(val tokenIterator: TokenIterator) extends AnyVal {
    def readJson[A](implicit jsonReader: JsonReader[A]): Either[ReaderError, A] = {
      jsonReader.read(tokenIterator)(FieldName())
    }
  }
}
