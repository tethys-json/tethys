package tethys.readers.tokens

import tethys.commons.Token
import tethys.readers.tokens.TokenIterator.CopySupport

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

  def collectExpression(): TokenIterator with CopySupport

}

object TokenIterator {
  trait CopySupport {
    def copy(): TokenIterator with CopySupport
  }
}
