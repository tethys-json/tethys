package tethys.jackson

import java.io.Writer

import com.fasterxml.jackson.core.JsonFactory
import tethys.readers.tokens.TokenIteratorProducer
import tethys.writers.tokens.{TokenWriter, TokenWriterProducer}

package object pretty {
  implicit def prettyJacksonTokenWriterProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): JacksonTokenWriterProducer =
    new tethys.jackson.JacksonTokenWriterProducer(jsonFactory, _.useDefaultPrettyPrinter())

  implicit def jacksonTokenIteratorProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): TokenIteratorProducer =
    tethys.jackson.jacksonTokenIteratorProducer
}
