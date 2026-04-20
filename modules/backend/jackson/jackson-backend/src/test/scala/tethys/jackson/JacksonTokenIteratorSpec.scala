package tethys.jackson

import tethys.readers.tokens.{TokenIteratorProducer, TokenIteratorSpec}

class JacksonTokenIteratorSpec extends TokenIteratorSpec {
  override def producer: TokenIteratorProducer =
    jacksonTokenIteratorProducer
}
