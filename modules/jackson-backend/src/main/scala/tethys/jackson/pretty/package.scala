package tethys.jackson

import java.io.Writer

import com.fasterxml.jackson.core.JsonFactory
import tethys.core.writers.tokens.{TokenWriter, TokenWriterProducer}

package object pretty {
  implicit def prettyJacksonTokenWriterProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenWriterProducer = new TokenWriterProducer {
    override def forWriter(writer: Writer): TokenWriter = {
      new JacksonTokenWriter(jsonFactory.createGenerator(writer).useDefaultPrettyPrinter())
    }
  }
}
