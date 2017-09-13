package tethys.core.readers.tokens

import com.fasterxml.jackson.core.{JsonParser, JsonToken}

trait TokenIterator {

  def next(): this.type

  def currentToken(): Token

  def nextToken(): Token

  def fieldName(): Option[String]

  def string(): Option[String]

  def number(): Option[Number]

  def boolean(): Option[Boolean]

  def skipExpression(): this.type

  def collectExpression(): TokenIterator

}

object TokenIterator {
  def fresh(jsonParser: JsonParser): TokenIterator = {
    jsonParser.nextToken()
    new JsonParserTokenIterator(jsonParser)
  }
}
