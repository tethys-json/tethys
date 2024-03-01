package tethys.jackson

import com.fasterxml.jackson.core.{JsonParser, JsonTokenId}
import tethys.commons.Token
import tethys.commons.Token._
import tethys.readers.tokens.{BaseTokenIterator, TokenIterator}

import scala.annotation.switch

final class JacksonTokenIterator(jsonParser: JsonParser)
    extends BaseTokenIterator {
  private[this] var token: Token = fromId(jsonParser.currentTokenId())
  override def currentToken(): Token = token

  override def nextToken(): Token = {
    val t = jsonParser.nextToken()
    token = {
      if (t == null) Token.Empty
      else fromId(t.id())
    }
    token
  }

  override def fieldName(): String = jsonParser.getCurrentName

  override def string(): String = jsonParser.getValueAsString()

  override def number(): Number = jsonParser.getNumberValue

  override def short(): Short = jsonParser.getShortValue

  override def int(): Int = jsonParser.getIntValue

  override def long(): Long = jsonParser.getLongValue

  override def float(): Float = jsonParser.getFloatValue

  override def double(): Double = jsonParser.getDoubleValue

  override def boolean(): Boolean = jsonParser.getBooleanValue

  private def fromId(tokenId: Int): Token = (tokenId: @switch) match {
    case JsonTokenId.ID_START_OBJECT => ObjectStartToken
    case JsonTokenId.ID_END_OBJECT   => ObjectEndToken
    case JsonTokenId.ID_START_ARRAY  => ArrayStartToken
    case JsonTokenId.ID_END_ARRAY    => ArrayEndToken
    case JsonTokenId.ID_FIELD_NAME   => FieldNameToken
    case JsonTokenId.ID_STRING       => StringValueToken
    case JsonTokenId.ID_NUMBER_INT   => NumberValueToken
    case JsonTokenId.ID_NUMBER_FLOAT => NumberValueToken
    case JsonTokenId.ID_TRUE         => BooleanValueToken
    case JsonTokenId.ID_FALSE        => BooleanValueToken
    case JsonTokenId.ID_NULL         => NullValueToken
    case _                           => Token.Empty
  }
}

object JacksonTokenIterator {
  def fromFreshParser(parser: JsonParser): TokenIterator = {
    parser.nextToken() // move parser to first token
    new JacksonTokenIterator(parser)
  }
}
