package tethys.writers.tokens

trait TokenWriterProducer {
  def produce(): TokenWriter
}
