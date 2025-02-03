import java.io.{Reader, StringReader}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{
  DefaultTokenWriter,
  TokenWriter,
  TokenWriterConfig,
  TokenWriterProducer
}

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
    ): String = {
      val tokenWriter = tokenWriterProducer.produce()
      try jsonWriter.write(a, tokenWriter)
      finally tokenWriter.flush()
      tokenWriter.result()
    }

    def asJsonWith(
        jsonWriter: JsonWriter[A]
    )(implicit tokenWriterProducer: TokenWriterProducer): String = {
      val tokenWriter = tokenWriterProducer.produce()
      try jsonWriter.write(a, tokenWriter)
      finally tokenWriter.flush()
      tokenWriter.result()
    }

    def writeJson(
        tokenWriter: TokenWriter
    )(implicit jsonWriter: JsonWriter[A]): Unit = {
      try jsonWriter.write(a, tokenWriter)
      finally
        tokenWriter.flush()
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

  implicit val defaultTokenWriterProducer: TokenWriterProducer =
    new TokenWriterProducer {
      type ExactTokenWriter = DefaultTokenWriter

      private val writerPool: ThreadLocal[DefaultTokenWriter] =
        new ThreadLocal[DefaultTokenWriter] {
          override def initialValue(): DefaultTokenWriter =
            new DefaultTokenWriter(config = TokenWriterConfig.Default)
        }

      override def produce(): TokenWriter = writerPool.get()
    }

}
