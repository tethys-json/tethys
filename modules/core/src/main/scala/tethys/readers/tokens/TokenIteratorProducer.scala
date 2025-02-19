package tethys.readers.tokens

import java.io.Reader

import tethys.readers.ReaderError

trait TokenIteratorProducer:
  def fromReader(reader: Reader): Either[ReaderError, TokenIterator]
