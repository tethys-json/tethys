package tethys.core.readers.tokens

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
