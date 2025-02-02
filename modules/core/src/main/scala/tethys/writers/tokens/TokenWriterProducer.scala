package tethys.writers.tokens

trait TokenWriterProducer {
  type ExactTokenWriter <: TokenWriter
  def withTokenWriter(writer: ExactTokenWriter => Unit): String
}
