package tethys

import java.io.{Reader, Writer}

import com.fasterxml.jackson.core.JsonFactory
import tethys.core.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.core.writers.tokens.{TokenWriter, TokenWriterProducer}

package object jackson {
  lazy val defaultJsonFactory: JsonFactory = new JsonFactory()

  implicit def jacksonTokenWriterProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenWriterProducer = new TokenWriterProducer {
    override def forWriter(writer: Writer): TokenWriter = {
      new JacksonTokenWriter(jsonFactory.createGenerator(writer))
    }
  }

  implicit def jacksonTokenIteratorProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenIteratorProducer = new TokenIteratorProducer {
    override def fromReader(reader: Reader): TokenIterator = {
      JacksonTokenIterator.fromFreshParser(jsonFactory.createParser(reader))
    }
  }
}
