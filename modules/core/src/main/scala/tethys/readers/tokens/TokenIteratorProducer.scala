package tethys.readers.tokens

import java.io.Reader

trait TokenIteratorProducer {
  def fromReader(reader: Reader): TokenIterator
}
