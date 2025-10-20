import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.readers.{FieldName, ReaderError}
import tethys.writers.tokens.{
  TokenWriter,
  TokenWriterConfig,
  TokenWriterProducer
}

import java.io.{Reader, StringReader, StringWriter}
import scala.Specializable.Group

package object tethys {

  final val specializations = new Group(
    (Byte, Short, Int, Long, Float, Double, Boolean)
  )

  implicit class JsonWriterOps[A](val a: A) extends AnyVal {
    def asJson(implicit
        jsonWriter: JsonWriter[A],
        tokenWriterProducer: TokenWriterProducer,
        tokenWriterConfig: TokenWriterConfig
    ): String = {
      val tokenWriter = tokenWriterProducer.produce(tokenWriterConfig)
      try jsonWriter.write(a, tokenWriter)
      finally tokenWriter.flush()
      tokenWriter.result()
    }

    def asJsonWith(
        jsonWriter: JsonWriter[A]
    )(implicit
        tokenWriterProducer: TokenWriterProducer,
        tokenWriterConfig: TokenWriterConfig
    ): String = {
      val tokenWriter = tokenWriterProducer.produce(tokenWriterConfig)
      try
        jsonWriter.write(a, tokenWriter)
      finally
        tokenWriter.flush()
      tokenWriter.result()
    }

    def writeJson(
        tokenWriter: TokenWriter
    )(implicit jsonWriter: JsonWriter[A]): Unit = {
      try
        jsonWriter.write(a, tokenWriter)
      finally
        tokenWriter.flush()
    }
  }

  implicit class StringReaderOps(val json: String) extends AnyVal {
    def jsonAs[A](implicit
        jsonReader: JsonReader[A],
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      implicit val fieldName: FieldName = FieldName.Root
      producer
        .produce(json)
        .flatMap(it => ReaderError.catchNonFatal(jsonReader.read(it)))
    }

    def toTokenIterator(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, TokenIterator] = producer.produce(json)
  }

  implicit class ReaderReaderOps(val reader: Reader) extends AnyVal {
    def readJson[A](implicit
        jsonReader: JsonReader[A],
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      implicit val root: FieldName = FieldName.Root
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
      implicit val fieldName: FieldName = FieldName.Root
      ReaderError.catchNonFatal(jsonReader.read(tokenIterator))
    }
  }
}
