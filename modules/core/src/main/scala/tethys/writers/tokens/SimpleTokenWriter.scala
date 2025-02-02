package tethys.writers.tokens

import tethys.commons.TokenNode
import tethys.commons.TokenNode._
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIteratorProducer

import scala.collection.mutable

class SimpleTokenWriter extends TokenWriter with TokenWriter.Flushing {
  val tokens: mutable.ArrayBuffer[TokenNode] = mutable.ArrayBuffer.empty

  override def writeArrayStart(): SimpleTokenWriter.this.type = append(
    ArrayStartNode
  )

  override def writeArrayEnd(): SimpleTokenWriter.this.type = append(
    ArrayEndNode
  )

  override def writeObjectStart(): SimpleTokenWriter.this.type = append(
    ObjectStartNode
  )

  override def writeObjectEnd(): SimpleTokenWriter.this.type = append(
    ObjectEndNode
  )

  override def writeFieldName(name: String): SimpleTokenWriter.this.type =
    append(FieldNameNode(name))

  override def writeString(v: String): SimpleTokenWriter.this.type = append(
    StringValueNode(v)
  )

  override def writeNumber(v: Byte): SimpleTokenWriter.this.type = append(
    ByteValueNode(v)
  )

  override def writeNumber(v: Short): SimpleTokenWriter.this.type = append(
    ShortValueNode(v)
  )

  override def writeNumber(v: Int): SimpleTokenWriter.this.type = append(
    IntValueNode(v)
  )

  override def writeNumber(v: Long): SimpleTokenWriter.this.type = append(
    LongValueNode(v)
  )

  override def writeNumber(v: BigInt): SimpleTokenWriter.this.type = append(
    NumberValueNode(v)
  )

  override def writeNumber(v: Double): SimpleTokenWriter.this.type = append(
    DoubleValueNode(v)
  )

  override def writeNumber(v: Float): SimpleTokenWriter.this.type = append(
    FloatValueNode(v)
  )

  override def writeNumber(v: BigDecimal): SimpleTokenWriter.this.type = append(
    NumberValueNode(v)
  )

  override def writeBoolean(v: Boolean): SimpleTokenWriter.this.type = append(
    BooleanValueNode(v)
  )

  override def writeNull(): SimpleTokenWriter.this.type = append(NullValueNode)

  override def writeRawJson(json: String): SimpleTokenWriter.this.type =
    throw new UnsupportedOperationException("SimpleTokenWriter.writeRawJson")

  override def close(): Unit = ()

  override def flush(): Unit = ()

  private def append(node: TokenNode): this.type = {
    tokens += node
    this
  }

  def withRawJsonSupport(implicit
      producer: TokenIteratorProducer
  ): SimpleTokenWriter = new SimpleTokenWriter {
    import tethys._
    override def writeRawJson(json: String): this.type = {
      val tokenIterator = json.toTokenIterator.fold(throw _, identity)
      JsonStreaming.streamValue(tokenIterator, this)(FieldName())
      this
    }
  }
}

object SimpleTokenWriter {
  implicit class SimpleTokenWriterOps[A](val a: A) extends AnyVal {
    import tethys._

    def asTokenList(implicit jsonWriter: JsonWriter[A]): List[TokenNode] = {
      val tokenWriter = new SimpleTokenWriter
      a.writeJson(tokenWriter)
      tokenWriter.tokens.toList
    }
  }
}
