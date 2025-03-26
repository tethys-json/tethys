package tethys.writers.tokens

trait TokenWriterProducer {
  def produce(config: TokenWriterConfig): TokenWriter
}

object TokenWriterProducer {
  implicit val tethysTokenWriterProducer: TokenWriterProducer =
    new TokenWriterProducer {
      private val writerPool: ThreadLocal[DefaultTokenWriter] =
        new ThreadLocal[DefaultTokenWriter] {
          override def initialValue(): DefaultTokenWriter =
            new DefaultTokenWriter(config = TokenWriterConfig.default)
        }

      override def produce(config: TokenWriterConfig): TokenWriter =
        writerPool.get().withConfig(config)

    }
}
