package tethys.core.writers.tokens
import java.math.BigInteger

import tethys.core.writers.tokens.SimpleTokenWriter._

import scala.collection.mutable

class SimpleTokenWriter extends TokenWriter {
  val tokens: mutable.ArrayBuffer[SimpleToken] = mutable.ArrayBuffer.empty

  override def writeArrayStart(): SimpleTokenWriter.this.type = append(ArrayStart)

  override def writeArrayEnd(): SimpleTokenWriter.this.type = append(ArrayEnd)

  override def writeObjectStart(): SimpleTokenWriter.this.type = append(ObjectStart)

  override def writeObjectEnd(): SimpleTokenWriter.this.type = append(ObjectEnd)

  override def writeFieldName(name: String): SimpleTokenWriter.this.type = append(FieldNameToken(name))

  override def writeString(v: String): SimpleTokenWriter.this.type = append(StringToken(v))

  override def writeNumber(v: Short): SimpleTokenWriter.this.type = append(ShortToken(v))

  override def writeNumber(v: Int): SimpleTokenWriter.this.type = append(IntToken(v))

  override def writeNumber(v: Long): SimpleTokenWriter.this.type = append(LongToken(v))

  override def writeNumber(v: BigInt): SimpleTokenWriter.this.type = append(BigIntegerToken(v))

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

  case object ArrayStart extends SimpleToken
  case object ArrayEnd extends SimpleToken
  case object ObjectStart extends SimpleToken
  case object ObjectEnd extends SimpleToken
  case class FieldNameToken(name: String) extends SimpleToken
  case object Null extends ValueToken
  case class StringToken(value: String) extends ValueToken
  case class ShortToken(value: Short) extends ValueToken
  case class IntToken(value: Int) extends ValueToken
  case class LongToken(value: Long) extends ValueToken
  case class BigIntegerToken(value: BigInt) extends ValueToken
  case class DoubleToken(value: Double) extends ValueToken
  case class FloatToken(value: Float) extends ValueToken
  case class BigDecimalToken(value: BigDecimal) extends ValueToken
  case class BooleanToken(value: Boolean) extends ValueToken

  implicit class SimpleTokenWriterOps[A](val a: A) extends AnyVal {
    import tethys.core.writers._

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

    ObjectStart :: tokens ::: ObjectEnd :: Nil
  }

  def arr(elems: Any*): List[SimpleToken] = {
    ArrayStart :: elems.toList.flatMap(anyToTokens) ::: ArrayEnd :: Nil
  }

  def value(v: String): List[ValueToken] = StringToken(v) :: Nil
  def value(v: Short): List[ValueToken] = ShortToken(v) :: Nil
  def value(v: Int): List[ValueToken] = IntToken(v) :: Nil
  def value(v: Long): List[ValueToken] = LongToken(v) :: Nil
  def value(v: BigInt): List[ValueToken] = BigIntegerToken(v) :: Nil
  def value(v: Double): List[ValueToken] = DoubleToken(v) :: Nil
  def value(v: Float): List[ValueToken] = FloatToken(v) :: Nil
  def value(v: BigDecimal): List[ValueToken] = BigDecimalToken(v) :: Nil
  def value(v: Boolean): List[ValueToken] = BooleanToken(v) :: Nil

  private def anyToTokens(any: Any): List[SimpleToken] = any match {
    case v: SimpleToken => v :: Nil
    case tokens: List[_] => tokens.flatMap(anyToTokens)
    case value: String => StringToken(value) :: Nil
    case value: Short => ShortToken(value) :: Nil
    case value: Int => IntToken(value) :: Nil
    case value: Long => LongToken(value) :: Nil
    case value: BigInteger => BigIntegerToken(value) :: Nil
    case value: Double => DoubleToken(value) :: Nil
    case value: Float => FloatToken(value) :: Nil
    case value: BigDecimal => BigDecimalToken(value) :: Nil
    case value: Boolean => BooleanToken(value) :: Nil
    case value => throw new Exception(s"Can't auto wrap '$value'")
  }
}
