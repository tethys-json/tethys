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

object TokenIteratorProducer {
  implicit val tethysTokenIteratorProducer: TokenIteratorProducer =
    new TokenIteratorProducer {
      private val readerPool: ThreadLocal[DefaultTokenIterator] =
        new ThreadLocal[DefaultTokenIterator] {
          override def initialValue(): DefaultTokenIterator =
            new DefaultTokenIterator(config = ReaderConfig.default)
        }
        
      override def produce(json: String): Either[ReaderError, TokenIterator] =
        Right(readerPool.get.init(json))
    }
}
