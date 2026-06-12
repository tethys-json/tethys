import java.io.{Reader, StringReader, StringWriter, Writer}

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
        jsonWriter: JsonWriter[A]^,
        tokenWriterProducer: TokenWriterProducer
    ): String = {
      WriterEntryPoints.render(a, jsonWriter)
    }

    def asJsonWith(
        jsonWriter: JsonWriter[A]^
    )(implicit tokenWriterProducer: TokenWriterProducer): String = {
      asJson(using jsonWriter, tokenWriterProducer)
    }

    def writeJson(
        tokenWriter: TokenWriter^
    )(implicit jsonWriter: JsonWriter[A]^): Unit = {
      WriterEntryPoints.writeToTokenWriter(a, jsonWriter, tokenWriter)
    }
  }

  implicit class WriterOps(val w: Writer) extends AnyVal {
    def withTokenWriter[A](op: TokenWriter^ => A)(implicit
        tokenWriterProducer: TokenWriterProducer
    ): A = {
      WriterEntryPoints.withScopedTokenWriter(w)(op)
    }

    @deprecated(
      "Use withTokenWriter instead. It is the scoped and capability-safe",
      "0.0.0"
    )
    def toTokenWriter(implicit
        tokenWriterProducer: TokenWriterProducer
    ): TokenWriter^ = {
      WriterEntryPoints.exportTokenWriter(w)
    }
  }

  implicit class StringReaderOps(val json: String) extends AnyVal {
    def jsonAs[A](implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      ReaderEntryPoints.decodeFromReader(new StringReader(json), jsonReader)
    }

    def withTokenIterator[A](op: TokenIterator^ => A)(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      ReaderEntryPoints.withScopedTokenIterator(new StringReader(json))(op)
    }

    def withJsonCursor[A, B](op: JsonStreaming.JsonCursor[A]^ => B)(implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[ReaderError, B] = {
      ReaderEntryPoints.withCursorFromReader(
        new StringReader(json),
        jsonReader
      )(
        op
      )
    }

    def foldJsonStream[A, S](zero: S)(f: (S, A) => S)(implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[
      ReaderError,
      S
    ] = {
      ReaderEntryPoints.foldStreamFromReader(
        new StringReader(json),
        jsonReader,
        zero
      )(f)
    }

    def foreachDecoded[A](f: A => Unit)(implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[ReaderError, Unit] = {
      ReaderEntryPoints.foreachDecodedFromReader(
        new StringReader(json),
        jsonReader
      )(f)
    }

    @deprecated(
      "Use withTokenIterator instead. It is the scoped and capability-safe",
      "0.0.0"
    )
    def toTokenIterator(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, TokenIterator^] = {
      ReaderEntryPoints.exportTokenIterator(new StringReader(json))
    }
  }

  implicit class ReaderReaderOps(val reader: Reader) extends AnyVal {
    def withTokenIterator[A](op: TokenIterator^ => A)(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      ReaderEntryPoints.withScopedTokenIterator(reader)(op)
    }

    def withJsonCursor[A, B](op: JsonStreaming.JsonCursor[A]^ => B)(implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[ReaderError, B] = {
      ReaderEntryPoints.withCursorFromReader(reader, jsonReader)(op)
    }

    def foldJsonStream[A, S](zero: S)(f: (S, A) => S)(implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[
      ReaderError,
      S
    ] = {
      ReaderEntryPoints.foldStreamFromReader(reader, jsonReader, zero)(f)
    }

    def foreachDecoded[A](f: A => Unit)(implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[ReaderError, Unit] = {
      ReaderEntryPoints.foreachDecodedFromReader(reader, jsonReader)(f)
    }

    def readJson[A](implicit
        jsonReader: JsonReader[A]^,
        producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      ReaderEntryPoints.decodeFromReader(reader, jsonReader)
    }

    def readJsonWith[A](
        jsonReader: JsonReader[A]^
    )(implicit producer: TokenIteratorProducer): Either[ReaderError, A] = {
      readJson[A](using jsonReader, producer)
    }

    @deprecated(
      "Use withTokenIterator instead. It is the scoped, capability-safe replacement for exporting TokenIterator.",
      "0.0.0"
    )
    def toTokenIterator(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, TokenIterator^] = {
      ReaderEntryPoints.exportTokenIterator(reader)
    }
  }

  implicit class TokenIteratorOps(private val tokenIterator: TokenIterator)
      extends AnyVal {
    def readJson[A](implicit
        jsonReader: JsonReader[A]^
    ): Either[ReaderError, A] = {
      TokenIteratorReadSupport.readJson(tokenIterator, jsonReader)
    }
  }

  object TokenIteratorSyntax {
    extension (tokenIterator: TokenIterator^)
      def readJson[A](implicit
          jsonReader: JsonReader[A]^
      ): Either[ReaderError, A] = {
        TokenIteratorReadSupport.readJson(tokenIterator, jsonReader)
      }
  }

  private[tethys] object TokenIteratorReadSupport {
    def readJson[A](
        tokenIterator: TokenIterator^,
        jsonReader: JsonReader[A]^
    ): Either[ReaderError, A] = {
      implicit val fieldName: FieldName = FieldName()
      ReaderError.catchNonFatal(jsonReader.read(tokenIterator))
    }
  }

  private object WriterEntryPoints {
    def render[A](
        value: A,
        jsonWriter: JsonWriter[A]^
    )(implicit tokenWriterProducer: TokenWriterProducer): String = {
      val stringWriter = new StringWriter()
      withScopedTokenWriter(stringWriter) { tokenWriter =>
        jsonWriter.write(value, tokenWriter)
      }
      stringWriter.toString
    }

    def writeToTokenWriter[A](
        value: A,
        jsonWriter: JsonWriter[A]^,
        tokenWriter: TokenWriter^
    ): Unit = {
      try jsonWriter.write(value, tokenWriter)
      finally {
        tokenWriter.flush()
      }
    }

    def withScopedTokenWriter[A](writer: Writer)(op: TokenWriter^ => A)(implicit
        tokenWriterProducer: TokenWriterProducer
    ): A = {
      val boxedTokenWriter: TokenWriter^ = tokenWriterProducer.forWriter(writer)
      try op(boxedTokenWriter)
      finally {
        boxedTokenWriter.flush()
      }
    }

    def exportTokenWriter(writer: Writer)(implicit
        tokenWriterProducer: TokenWriterProducer
    ): TokenWriter^ = {
      val boxedTokenWriter: TokenWriter^ = tokenWriterProducer.forWriter(writer)
      boxedTokenWriter
    }
  }

  private object ReaderEntryPoints {
    def withScopedTokenIterator[A](reader: Reader)(op: TokenIterator^ => A)(
        implicit producer: TokenIteratorProducer
    ): Either[ReaderError, A] = {
      producer.fromReader(reader) match {
        case Right(tokenIterator) =>
          val boxedTokenIterator: TokenIterator^ = tokenIterator
          Right(op(boxedTokenIterator))
        case Left(error) => Left(error)
      }
    }

    def decodeFromReader[A](
        reader: Reader,
        jsonReader: JsonReader[A]^
    )(implicit producer: TokenIteratorProducer): Either[ReaderError, A] = {
      withScopedTokenIterator(reader) { tokenIterator =>
        TokenIteratorReadSupport.readJson(tokenIterator, jsonReader)
      } match {
        case Right(result) => result
        case Left(error)   => Left(error)
      }
    }

    def withCursorFromReader[A, B](
        reader: Reader,
        jsonReader: JsonReader[A]^
    )(op: JsonStreaming.JsonCursor[A]^ => B)(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, B] = {
      withScopedTokenIterator(reader) { tokenIterator =>
        given FieldName = FieldName()

        ReaderError.catchNonFatal {
          JsonStreaming.withJsonCursor[A, B](tokenIterator)(op)(using jsonReader)
        }
      } match {
        case Right(result) => result
        case Left(error)   => Left(error)
      }
    }

    def foldStreamFromReader[A, S](
        reader: Reader,
        jsonReader: JsonReader[A]^,
        zero: S
    )(f: (S, A) => S)(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, S] = {
      withScopedTokenIterator(reader) { tokenIterator =>
        given FieldName = FieldName()

        ReaderError.catchNonFatal {
          JsonStreaming.foldJsonStream[A, S](
            tokenIterator,
            zero
          )(f)(using jsonReader)
        }
      } match {
        case Right(result) => result
        case Left(error)   => Left(error)
      }
    }

    def foreachDecodedFromReader[A](
        reader: Reader,
        jsonReader: JsonReader[A]^
    )(f: A => Unit)(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, Unit] = {
      withScopedTokenIterator(reader) { tokenIterator =>
        given FieldName = FieldName()

        ReaderError.catchNonFatal {
          JsonStreaming.foreachDecoded[A](tokenIterator)(f)(using jsonReader)
        }
      } match {
        case Right(result) => result
        case Left(error)   => Left(error)
      }
    }

    def exportTokenIterator(reader: Reader)(implicit
        producer: TokenIteratorProducer
    ): Either[ReaderError, TokenIterator^] = {
      producer.fromReader(reader) match {
        case Right(tokenIterator) =>
          val boxedTokenIterator: TokenIterator^ = tokenIterator
          Right(boxedTokenIterator)
        case Left(error) => Left(error)
      }
    }
  }
}
