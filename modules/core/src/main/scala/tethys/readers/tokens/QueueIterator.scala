package tethys.readers.tokens

import tethys.readers.tokens.QueueIterator.TokenNode

import scala.collection.mutable

class QueueIterator(nodes: mutable.Queue[TokenNode]) extends BaseTokenIterator {
  override def currentToken(): Token = if(nodes.isEmpty) Token.Empty else nodes.front.token

  override def nextToken(): Token = if(nodes.isEmpty) Token.Empty else {
    nodes.dequeue()
    currentToken()
  }

  //TODO
  override def fieldName(): String = ??? //nodes.front.string

  override def string(): String = ??? //nodes.front.string

  override def number(): Number = ??? //nodes.front.number

  override def boolean(): Boolean = ??? //nodes.front.boolean

  override def short(): Short = ???

  override def int(): Int = ???

  override def long(): Long = ???

  override def float(): Float = ???

  override def double(): Double = ???
}

object QueueIterator {
  def apply(nodes: Seq[TokenNode]): QueueIterator = new QueueIterator(mutable.Queue[TokenNode](nodes:_*))

  case class TokenNode(token: Token,
                       string: Option[String] = None,
                       number: Option[Number] = None,
                       boolean: Option[Boolean] = None)

  object TokenNode {
    def string(token: Token, string: String): TokenNode = TokenNode(token, string = Some(string))
    def number(token: Token, number: Number): TokenNode = TokenNode(token, number = Some(number))
    def boolean(token: Token, boolean: Boolean): TokenNode = TokenNode(token, boolean = Some(boolean))
  }
}
