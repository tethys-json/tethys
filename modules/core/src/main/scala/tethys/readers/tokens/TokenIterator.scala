package tethys.readers.tokens

trait TokenIterator {

  def next(): this.type

  def currentToken(): Token

  def nextToken(): Token

  def fieldName(): String

  def string(): String

  def number(): Number

  def short(): Short

  def int(): Int

  def long(): Long

  def float(): Float

  def double(): Double

  def boolean(): Boolean

  def skipExpression(): this.type

  def collectExpression(): TokenIterator

}
