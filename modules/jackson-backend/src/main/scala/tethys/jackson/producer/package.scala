package tethys.jackson

import java.io.Writer

import com.fasterxml.jackson.core.JsonFactory
import tethys.core.writers.token.{TokenWriter, TokenWriterProducer}

package object producer {
  lazy val defaultJsonFactory: JsonFactory = new JsonFactory()

  implicit def jacksonTokenWriterProducer(implicit jsonFactory: JsonFactory = defaultJsonFactory): TokenWriterProducer = new TokenWriterProducer {
    override def forWriter(writer: Writer): TokenWriter = {
      new JacksonTokenWriter(jsonFactory.createGenerator(writer))
    }
  }
}
