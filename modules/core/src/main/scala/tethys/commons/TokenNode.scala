package tethys.commons

import tethys.JsonReader
import tethys.commons.Token._
import tethys.readers.ReaderError
import tethys.readers.tokens.{QueueIterator, TokenIteratorProducer}

sealed trait TokenNode {
  def token: Token
}

object TokenNode {

  case object NullValueNode extends TokenNode {
    override val token: Token = NullValueToken
  }
  case object ArrayStartNode extends TokenNode {
    override val token: Token = ArrayStartToken
  }
  case object ArrayEndNode extends TokenNode {
    override val token: Token = ArrayEndToken
  }
  case object ObjectStartNode extends TokenNode {
    override val token: Token = ObjectStartToken
  }
  case object ObjectEndNode extends TokenNode {
    override val token: Token = ObjectEndToken
  }

  case class FieldNameNode(value: String) extends TokenNode {
    override val token: Token = FieldNameToken
  }
  case class StringValueNode(value: String) extends TokenNode {
    override val token: Token = StringValueToken
  }
  case class BooleanValueNode(value: Boolean) extends TokenNode {
    override val token: Token = BooleanValueToken
  }
  case class NumberValueNode(value: Number) extends TokenNode {
    override val token: Token = NumberValueToken
  }

  case class ShortValueNode(value: Short) extends TokenNode {
    override val token: Token = NumberValueToken
  }
  case class IntValueNode(value: Int) extends TokenNode {
    override val token: Token = NumberValueToken
  }
  case class LongValueNode(value: Long) extends TokenNode {
    override val token: Token = NumberValueToken
  }
  case class FloatValueNode(value: Float) extends TokenNode {
    override val token: Token = NumberValueToken
  }
  case class DoubleValueNode(value: Double) extends TokenNode {
    override val token: Token = NumberValueToken
  }


  def obj(fields: (String, Any)*): List[TokenNode] = {
    val tokens = fields.toList.flatMap {
      case (name, a) => FieldNameNode(name) :: anyToTokens(a)
    }

    ObjectStartNode :: tokens ::: ObjectEndNode :: Nil
  }

  def arr(elems: Any*): List[TokenNode] = {
    ArrayStartNode :: elems.toList.flatMap(anyToTokens) ::: ArrayEndNode :: Nil
  }

  def value(v: String): List[TokenNode] = StringValueNode(v) :: Nil
  def value(v: Boolean): List[TokenNode] = BooleanValueNode(v) :: Nil
  def value(v: Short): List[TokenNode] = ShortValueNode(v) :: Nil
  def value(v: Int): List[TokenNode] = IntValueNode(v) :: Nil
  def value(v: Long): List[TokenNode] = LongValueNode(v) :: Nil
  def value(v: Float): List[TokenNode] = FloatValueNode(v) :: Nil
  def value(v: Double): List[TokenNode] = DoubleValueNode(v) :: Nil
  def value(v: BigInt): List[TokenNode] = NumberValueNode(v) :: Nil
  def value(v: java.math.BigInteger): List[TokenNode] = NumberValueNode(v) :: Nil
  def value(v: BigDecimal): List[TokenNode] = NumberValueNode(v) :: Nil
  def value(v: java.math.BigDecimal): List[TokenNode] = NumberValueNode(v) :: Nil

  private def anyToTokens(any: Any): List[TokenNode] = any match {
    case v: TokenNode => v :: Nil
    case nodes: List[_] => nodes.flatMap(anyToTokens)
    case v: String => value(v)
    case v: Short => value(v)
    case v: Int => value(v)
    case v: Long => value(v)
    case v: java.math.BigInteger => value(v)
    case v: BigInt => value(v)
    case v: Double => value(v)
    case v: Float => value(v)
    case v: java.math.BigDecimal => value(v)
    case v: BigDecimal => value(v)
    case v: Boolean => value(v)
    case null | None => NullValueNode :: Nil
    case v => throw new Exception(s"Can't auto wrap '$v'")
  }

  implicit class TokenNodesOps(val json: String) extends AnyVal {
    def jsonAsTokensList(implicit producer: TokenIteratorProducer): List[TokenNode] = {
      import tethys._
      val iterator = json.toTokenIterator.fold(throw _, identity)
      val builder = List.newBuilder[TokenNode]
      while (!iterator.currentToken().isEmpty) {
        val token = iterator.currentToken()
        val node = {
          if (token.isArrayStart) ArrayStartNode
          else if (token.isArrayEnd) ArrayEndNode
          else if (token.isObjectStart) ObjectStartNode
          else if (token.isObjectEnd) ObjectEndNode
          else if (token.isNullValue) NullValueNode
          else if (token.isFieldName) FieldNameNode(iterator.fieldName())
          else if (token.isStringValue) StringValueNode(iterator.string())
          else if (token.isNumberValue) iterator.number() match {
            case v: java.lang.Short => ShortValueNode(v)
            case v: java.lang.Integer => IntValueNode(v)
            case v: java.lang.Long => LongValueNode(v)
            case v: java.lang.Float => FloatValueNode(v)
            case v: java.lang.Double => DoubleValueNode(v)
            case n => NumberValueNode(n)
          }
          else BooleanValueNode(iterator.boolean())
        }

        builder += node
        iterator.next()
      }

      builder.result()
    }
  }

  implicit class TokenListOps(private val tokens: Seq[TokenNode]) extends AnyVal {
    import tethys.TokenIteratorOps
    def tokensAs[A: JsonReader]: A = QueueIterator(tokens).readJson[A].fold(throw _, identity)
  }
}
