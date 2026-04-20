package tethys.readers.tokens

import java.io.{Reader, StringWriter}
import tethys.readers.ReaderError

trait TokenIteratorProducer {
  def produce(json: String): Either[ReaderError, TokenIterator]

  def fromReader(reader: Reader): Either[ReaderError, TokenIterator] = {
    val writer = new StringWriter()
    reader.transferTo(writer)
    val json = writer.toString
    reader.close()
    writer.close()
    produce(json)
  }
}
