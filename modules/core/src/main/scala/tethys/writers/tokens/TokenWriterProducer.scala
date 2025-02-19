package tethys.writers.tokens

import java.io.Writer

trait TokenWriterProducer:
  def forWriter(writer: Writer): TokenWriter
