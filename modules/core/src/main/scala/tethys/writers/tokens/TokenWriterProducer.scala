package tethys.writers.tokens

trait TokenWriterProducer {
  def produce(config: TokenWriterConfig): TokenWriter
}
