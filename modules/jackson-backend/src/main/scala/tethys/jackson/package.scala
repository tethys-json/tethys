package tethys

import java.io.{Reader, Writer}

import com.fasterxml.jackson.core.JsonFactory
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{TokenWriter, TokenWriterProducer}

package object jackson {
  lazy val defaultJsonFactory: JsonFactory = {
    val f = new JsonFactory()
    f.configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
    f
  }

  implicit def jacksonTokenWriterProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): TokenWriterProducer = new TokenWriterProducer {
    override def forWriter(writer: Writer): TokenWriter = {
      new JacksonTokenWriter(jsonFactory.createGenerator(writer))
    }
  }

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
