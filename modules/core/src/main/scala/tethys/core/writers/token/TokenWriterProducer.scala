package tethys.core.writers.token

import java.io.Writer

trait TokenWriterProducer {
  def forWriter(writer: Writer): TokenWriter
}
