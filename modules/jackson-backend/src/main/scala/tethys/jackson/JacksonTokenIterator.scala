package tethys.jackson

import com.fasterxml.jackson.core.{JsonParser, JsonToken}
import tethys.core.readers.tokens.{BaseTokenIterator, Token, TokenIterator}

class JacksonTokenIterator(jsonParser: JsonParser) extends BaseTokenIterator {
  override def currentToken(): Token = {
    val token = jsonParser.currentToken()
    if(token == null) Token.Empty else JacksonToken(token)
  }

  override def nextToken(): Token = {
    val token = jsonParser.nextToken()
    if(token == null) Token.Empty else JacksonToken(token)
  }

  override def fieldName(): Option[String] = Option(jsonParser.getCurrentName)

  override def string(): Option[String] = {
    if (jsonParser.currentToken() != JsonToken.VALUE_STRING) None
    else Option(jsonParser.getValueAsString())
  }

  override def number(): Option[Number] = {
    if (!jsonParser.currentToken().isNumeric) None
    else Option(jsonParser.getNumberValue)
  }

  override def boolean(): Option[Boolean] = {
    jsonParser.currentToken() match {
      case JsonToken.VALUE_TRUE => Some(true)
      case JsonToken.VALUE_FALSE => Some(false)
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