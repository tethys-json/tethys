package tethys.commons

import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.{
  TokenWriter,
  TokenWriterConfig,
  TokenWriterProducer
}
import tethys.{JsonReader, JsonStreaming, JsonWriter}

final case class RawJson(json: String)

object RawJson {
  implicit val rawJsonWriter: JsonWriter[RawJson] = new JsonWriter[RawJson] {
    override def write(value: RawJson, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeRawJson(value.json)
  }

  implicit def rawJsonReader(implicit
      tokenWriterProducer: TokenWriterProducer,
      tokenWriterConfig: TokenWriterConfig
  ): JsonReader[RawJson] = new JsonReader[RawJson] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): RawJson = {
      val tokenWriter = tokenWriterProducer.produce(tokenWriterConfig)
      try JsonStreaming.streamValue(it, tokenWriter)
      finally tokenWriter.flush()
      RawJson(tokenWriter.result())
    }
  }
}
