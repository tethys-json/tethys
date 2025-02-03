package tethys

import java.io.{Reader, Writer}

import com.fasterxml.jackson.core.{JsonFactory, JsonGenerator}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{TokenWriter, TokenWriterProducer}

package object jackson {
  lazy val defaultJsonFactory: JsonFactory = {
    val f = new JsonFactory()
    f.configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
    f
  }

  class JacksonTokenWriterProducer(
      jsonFactory: JsonFactory,
      configure: JsonGenerator => JsonGenerator
  ) extends TokenWriterProducer {
    override def produce(): TokenWriter =
      new JacksonTokenWriter(
        configure(jsonFactory.createGenerator(new java.io.StringWriter()))
      )
  }

  implicit def jacksonTokenWriterProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): JacksonTokenWriterProducer =
    new JacksonTokenWriterProducer(jsonFactory, identity)

  implicit def jacksonTokenIteratorProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): TokenIteratorProducer = new TokenIteratorProducer {
    override def fromReader(
        reader: Reader
    ): Either[ReaderError, TokenIterator] = {
      ReaderError.catchNonFatal(
        JacksonTokenIterator.fromFreshParser(jsonFactory.createParser(reader))
      )(FieldName())
    }
  }
}
