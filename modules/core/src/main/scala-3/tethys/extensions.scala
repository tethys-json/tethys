package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{
  DefaultTokenWriter,
  TokenWriter,
  TokenWriterConfig,
  TokenWriterProducer
}

import java.io.StringReader

val specializations = new scala.Specializable.Group(
  (Byte, Short, Int, Long, Float, Double, Boolean)
)

extension [A](a: A)
  def asJson(using
      jsonWriter: JsonWriter[A],
      tokenWriterProducer: TokenWriterProducer
  ): String =
    val tokenWriter = tokenWriterProducer.produce()
    try jsonWriter.write(a, tokenWriter)
    finally tokenWriter.flush()
    tokenWriter.result()

  def asJsonWith(
      jsonWriter: JsonWriter[A]
  )(using tokenWriterProducer: TokenWriterProducer): String =
    val tokenWriter = tokenWriterProducer.produce()
    try jsonWriter.write(a, tokenWriter)
    finally tokenWriter.flush()
    tokenWriter.result()

  def writeJson(
      tokenWriter: TokenWriter
  )(using
      jsonWriter: JsonWriter[A]
  ): Unit =
    try jsonWriter.write(a, tokenWriter)
    finally
      tokenWriter.flush()

extension (json: String)
  def jsonAs[A](using
      jsonReader: JsonReader[A],
      producer: TokenIteratorProducer
  ): Either[ReaderError, A] =
    new StringReader(json).readJson[A]

  def toTokenIterator(using
      producer: TokenIteratorProducer
  ): Either[ReaderError, TokenIterator] =
    new StringReader(json).toTokenIterator

extension (reader: java.io.Reader)
  def readJson[A](using
      jsonReader: JsonReader[A],
      producer: TokenIteratorProducer
  ): Either[ReaderError, A] =
    given FieldName = FieldName()
    producer.fromReader(reader).flatMap(_.readJson[A])

  def readJsonWith[A](
      jsonReader: JsonReader[A]
  )(using TokenIteratorProducer): Either[ReaderError, A] =
    readJson[A](using jsonReader)

  def toTokenIterator(using
      producer: TokenIteratorProducer
  ): Either[ReaderError, TokenIterator] =
    producer.fromReader(reader)

extension [T <: TokenIterator](tokenIterator: T)
  def readJson[A](using
      jsonReader: JsonReader[A]
  ): Either[ReaderError, A] =
    given FieldName = FieldName()
    ReaderError.catchNonFatal(jsonReader.read(tokenIterator))

given TokenWriterProducer with
  type ExactTokenWriter = DefaultTokenWriter

  private val writerPool: ThreadLocal[DefaultTokenWriter] =
    new ThreadLocal[DefaultTokenWriter] {
      override def initialValue(): DefaultTokenWriter =
        new DefaultTokenWriter(config = TokenWriterConfig.Default)
    }

  override def produce(): TokenWriter = writerPool.get()
