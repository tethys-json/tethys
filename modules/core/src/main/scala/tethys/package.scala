import java.io.{Reader, StringReader}

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{TokenWriter, TokenWriterProducer}

import scala.Specializable.Group

package object tethys {

  final val specializations = new Group(
    (Byte, Short, Int, Long, Float, Double, Boolean)
  )

  // given

  implicit class JsonWriterOps[A](val a: A) extends AnyVal {
    def asJson(implicit
        jsonWriter: JsonWriter[A],
        tokenWriterProducer: TokenWriterProducer
    ): String =
      tokenWriterProducer.withTokenWriter(jsonWriter.write(a, _))

    def asJsonWith(
        jsonWriter: JsonWriter[A]
    )(implicit tokenWriterProducer: TokenWriterProducer): String = {
      asJson(jsonWriter, tokenWriterProducer)
    }

    def writeJson(
        tokenWriter: TokenWriter with TokenWriter.Flushing
    )(implicit jsonWriter: JsonWriter[A]): Unit = {
      try jsonWriter.write(a, tokenWriter)
      finally {
        tokenWriter.flush()
      }
    }
  }

  implicit class StringReaderOps(val json: String) extends AnyVal {
    def jsonAs[A](implicit
        jsonReader: JsonReader[A],
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      new StringReader(json).readJson[A]
    }

    def toTokenIterator(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, TokenIterator] = {
      new StringReader(json).toTokenIterator
    }
  }

  implicit class ReaderReaderOps(val reader: Reader) extends AnyVal {
    def readJson[A](implicit
        jsonReader: JsonReader[A],
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      implicit val root: FieldName = FieldName()
      producer.fromReader(reader).right.flatMap(_.readJson[A])
    }

    def readJsonWith[A](
        jsonReader: JsonReader[A]
    )(implicit producer: TokenIteratorProducer): Either[ReaderError, A] = {
      readJson[A](jsonReader, producer)
    }

    def toTokenIterator(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, TokenIterator] = {
      producer.fromReader(reader)
    }
  }

  implicit class TokenIteratorOps(val tokenIterator: TokenIterator)
      extends AnyVal {
    def readJson[A](implicit
        jsonReader: JsonReader[A]
    ): Either[ReaderError, A] = {
      implicit val fieldName: FieldName = FieldName()
      ReaderError.catchNonFatal(jsonReader.read(tokenIterator))
    }
  }
}
