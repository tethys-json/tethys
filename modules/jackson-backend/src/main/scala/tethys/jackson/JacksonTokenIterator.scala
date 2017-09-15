package tethys.jackson

import com.fasterxml.jackson.core.{JsonParser, JsonToken, JsonTokenId}
import tethys.readers.tokens.{BaseTokenIterator, Token, TokenIterator}

class JacksonTokenIterator(jsonParser: JsonParser) extends BaseTokenIterator {
  private[this] var token: JacksonToken = JacksonToken(jsonParser.currentTokenId())


  override def currentToken(): Token = token

  override def nextToken(): Token = {
    jsonParser.nextToken()
    token = JacksonToken(jsonParser.currentTokenId())
    token
  }

  override def fieldName(): Option[String] = Option(jsonParser.getCurrentName)

  override def string(): Option[String] = {
    if (!token.isStringValue) None
    else Option(jsonParser.getValueAsString())
  }

  override def number(): Option[Number] = {
    if (!token.isNumberValue) None
    else Option(jsonParser.getNumberValue)
  }

  override def boolean(): Option[Boolean] = {
    token.tokenId match {
      case JsonTokenId.ID_TRUE => Some(true)
      case JsonTokenId.ID_FALSE => Some(false)
      case _ => None
    }
  }
}

object JacksonTokenIterator {
  def fromFreshParser(parser: JsonParser): TokenIterator = {
    parser.nextToken()// move parser to first token
    new JacksonTokenIterator(parser)
  }
}