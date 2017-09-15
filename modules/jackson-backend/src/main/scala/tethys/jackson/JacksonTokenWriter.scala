package tethys.jackson

import com.fasterxml.jackson.core.JsonGenerator
import tethys.writers.tokens.TokenWriter

class JacksonTokenWriter(jsonGenerator: JsonGenerator) extends TokenWriter {
  override def writeArrayStart(): JacksonTokenWriter.this.type = {
    jsonGenerator.writeStartArray()
    this
  }

  override def writeArrayEnd(): JacksonTokenWriter.this.type = {
    jsonGenerator.writeEndArray()
    this
  }

  override def writeObjectStart(): JacksonTokenWriter.this.type = {
    jsonGenerator.writeStartObject()
    this
  }

  override def writeObjectEnd(): JacksonTokenWriter.this.type = {
    jsonGenerator.writeEndObject()
    this
  }

  override def writeFieldName(name: String): JacksonTokenWriter.this.type = {
    jsonGenerator.writeFieldName(name)
    this
  }

  override def writeString(v: String): JacksonTokenWriter.this.type = {
    jsonGenerator.writeString(v)
    this
  }

  override def writeNumber(v: Short): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v)
    this
  }

  override def writeNumber(v: Int): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v)
    this
  }

  override def writeNumber(v: Long): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v)
    this
  }

  override def writeNumber(v: BigInt): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v.bigInteger)
    this
  }

  override def writeNumber(v: Double): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v)
    this
  }

  override def writeNumber(v: Float): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v)
    this
  }

  override def writeNumber(v: BigDecimal): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNumber(v.bigDecimal)
    this
  }

  override def writeBoolean(v: Boolean): JacksonTokenWriter.this.type = {
    jsonGenerator.writeBoolean(v)
    this
  }

  override def writeNull(): JacksonTokenWriter.this.type = {
    jsonGenerator.writeNull()
    this
  }

  override def close(): Unit = jsonGenerator.close()
}
