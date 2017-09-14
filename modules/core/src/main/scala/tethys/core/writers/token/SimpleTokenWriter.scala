package tethys.core.writers.token
import java.math.BigInteger

import tethys.core.writers.JsonWriter
import tethys.core.writers.token.SimpleTokenWriter._

import scala.collection.mutable

class SimpleTokenWriter extends TokenWriter {
  val tokens: mutable.ArrayBuffer[SimpleToken] = mutable.ArrayBuffer.empty

  override def writeStartArray(): SimpleTokenWriter.this.type = append(StartArray)

  override def writeEndArray(): SimpleTokenWriter.this.type = append(EndArray)

  override def writeStartObject(): SimpleTokenWriter.this.type = append(StartObject)

  override def writeEndObject(): SimpleTokenWriter.this.type = append(EndObject)

  override def writeFieldName(name: String): SimpleTokenWriter.this.type = append(FieldNameToken(name))

  override def writeString(v: String): SimpleTokenWriter.this.type = append(StringToken(v))

  override def writeNumber(v: Short): SimpleTokenWriter.this.type = append(ShortToken(v))

  override def writeNumber(v: Int): SimpleTokenWriter.this.type = append(IntToken(v))

  override def writeNumber(v: Long): SimpleTokenWriter.this.type = append(LongToken(v))

  override def writeNumber(v: BigInteger): SimpleTokenWriter.this.type = append(BigIntegerToken(v))

  override def writeNumber(v: Double): SimpleTokenWriter.this.type = append(DoubleToken(v))

  override def writeNumber(v: Float): SimpleTokenWriter.this.type = append(FloatToken(v))

  override def writeNumber(v: BigDecimal): SimpleTokenWriter.this.type = append(BigDecimalToken(v))

  override def writeBoolean(v: Boolean): SimpleTokenWriter.this.type = append(BooleanToken(v))

  override def writeNull(): SimpleTokenWriter.this.type = append(Null)

  override def close(): Unit = ()

  private def append(token: SimpleToken): this.type = {
    tokens += token
    this
  }
}

object SimpleTokenWriter {
  sealed trait SimpleToken
  sealed trait ValueToken extends SimpleToken

  case object StartArray extends SimpleToken
  case object EndArray extends SimpleToken
  case object StartObject extends SimpleToken
  case object EndObject extends SimpleToken
  case class FieldNameToken(name: String) extends SimpleToken
  case object Null extends ValueToken
  case class StringToken(value: String) extends ValueToken
  case class ShortToken(value: Short) extends ValueToken
  case class IntToken(value: Int) extends ValueToken
  case class LongToken(value: Long) extends ValueToken
  case class BigIntegerToken(value: BigInteger) extends ValueToken
  case class DoubleToken(value: Double) extends ValueToken
  case class FloatToken(value: Float) extends ValueToken
  case class BigDecimalToken(value: BigDecimal) extends ValueToken
  case class BooleanToken(value: Boolean) extends ValueToken

  implicit class SimpleTokenWriterOps[A](val a: A) extends AnyVal {
    import tethys.core.writers.syntax._

    def asTokenList(implicit jsonWriter: JsonWriter[A]): List[SimpleToken] = {
      val tokenWriter = new SimpleTokenWriter
      a.writeJson(tokenWriter)
      tokenWriter.tokens.toList
    }
  }

  def obj(fields: (String, Any)*): List[SimpleToken] = {
    val tokens = fields.toList.flatMap {
      case (name, a) => FieldNameToken(name) :: anyToTokens(a)
    }

    StartObject :: tokens ::: EndObject :: Nil
  }

  def arr(elems: Any*): List[SimpleToken] = {
    StartArray :: elems.toList.flatMap(anyToTokens) ::: EndArray :: Nil
  }

  private def anyToTokens(any: Any): List[SimpleToken] = any match {
    case tokens: List[SimpleToken] => tokens
    case v: ValueToken => v :: Nil
    case value: String => StringToken(value) :: Nil
    case value: Short => ShortToken(value) :: Nil
    case value: Int => IntToken(value) :: Nil
    case value: Long => LongToken(value) :: Nil
    case value: BigInteger => BigIntegerToken(value) :: Nil
    case value: Double => DoubleToken(value) :: Nil
    case value: Float => FloatToken(value) :: Nil
    case value: BigDecimal => BigDecimalToken(value) :: Nil
    case value: Boolean => BooleanToken(value) :: Nil
  }
}
