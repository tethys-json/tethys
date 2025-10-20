package tethys.jackson

import tethys.JsonReaderSpec
import tethys.readers.tokens.TokenIteratorProducer

class JacksonJsonReaderSpec extends JsonReaderSpec {
  def producer: TokenIteratorProducer = jacksonTokenIteratorProducer
}
