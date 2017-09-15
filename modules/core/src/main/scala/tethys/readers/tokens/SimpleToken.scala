package tethys.readers.tokens

import java.math.BigInteger

import tethys.readers.tokens.QueueIterator.TokenNode

sealed trait SimpleToken extends Token {
  override def isStringValue: Boolean = false
  override def isNumberValue: Boolean = false
  override def isBooleanValue: Boolean = false
  override def isNullValue: Boolean = false
  override def isFieldName: Boolean = false
  override def isArrayStart: Boolean = false
  override def isArrayEnd: Boolean = false
  override def isObjectStart: Boolean = false
  override def isObjectEnd: Boolean = false
  override def isEmpty: Boolean = false
}

object SimpleToken {
  case object StringValueToken extends SimpleToken {
    override def isStringValue: Boolean = true
  }
  case object NumberValueToken extends SimpleToken {
    override def isNumberValue: Boolean = true
  }
  case object BooleanValueToken extends SimpleToken {
    override def isBooleanValue: Boolean = true
  }
  case object NullValueToken extends SimpleToken {
    override def isNullValue: Boolean = true
  }
  case object FieldNameToken extends SimpleToken {
    override def isFieldName: Boolean = true
  }
  case object ArrayStartToken extends SimpleToken {
    override def isArrayStart: Boolean = true
  }
  case object ArrayEndToken extends SimpleToken {
    override def isArrayEnd: Boolean = true
  }
  case object ObjectStartToken extends SimpleToken {
    override def isObjectStart: Boolean = true
  }
  case object ObjectEndToken extends SimpleToken {
    override def isObjectEnd: Boolean = true
  }

  def obj(fields: (String, Any)*): List[TokenNode] = {
    val tokens = fields.toList.flatMap {
      case (name, a) => TokenNode.string(FieldNameToken, name) :: anyToTokens(a)
    }

    TokenNode(ObjectStartToken) :: tokens ::: TokenNode(ObjectEndToken) :: Nil
  }

  def arr(elems: Any*): List[TokenNode] = {
    TokenNode(ArrayStartToken) :: elems.toList.flatMap(anyToTokens) ::: TokenNode(ArrayEndToken) :: Nil
  }

  def value(v: String): List[TokenNode] = TokenNode.string(StringValueToken, v) :: Nil
  def value(v: Short): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: Int): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: Long): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: BigInt): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: Double): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: Float): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: BigDecimal): List[TokenNode] = TokenNode.number(NumberValueToken, v) :: Nil
  def value(v: Boolean): List[TokenNode] = TokenNode.boolean(BooleanValueToken, v) :: Nil

  private def anyToTokens(any: Any): List[TokenNode] = any match {
    case v: TokenNode => v :: Nil
    case nodes: List[_] => nodes.flatMap(anyToTokens)
    case v: String => value(v)
    case v: Short => value(v)
    case v: Int => value(v)
    case v: Long => value(v)
    case v: BigInteger => value(v)
    case v: Double => value(v)
    case v: Float => value(v)
    case v: BigDecimal => value(v)
    case v: Boolean => value(v)
    case null => TokenNode(NullValueToken) :: Nil
    case v => throw new Exception(s"Can't auto wrap '$v'")
  }
}
