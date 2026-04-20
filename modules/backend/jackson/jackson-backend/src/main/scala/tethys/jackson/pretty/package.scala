package tethys.jackson

import com.fasterxml.jackson.core.JsonFactory
import tethys.readers.tokens.TokenIteratorProducer

package object pretty {

  @deprecated("Provide implicit TokenWriterConfig to `asJson` instead")
  implicit def prettyJacksonTokenWriterProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): JacksonTokenWriterProducer =
    new tethys.jackson.JacksonTokenWriterProducer(
      jsonFactory,
      modifyConfig = _.withDefaultPrettyPrinter
    )

  implicit def jacksonTokenIteratorProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): TokenIteratorProducer =
    tethys.jackson.jacksonTokenIteratorProducer
}
