package tethys.readers.tokens

import tethys.readers.tokens.QueueIterator.TokenNode

import scala.collection.mutable

class QueueIterator(nodes: mutable.Queue[TokenNode]) extends BaseTokenIterator {
  override def currentToken(): Token = if(nodes.isEmpty) Token.Empty else nodes.front.token

  override def nextToken(): Token = if(nodes.isEmpty) Token.Empty else {
    nodes.dequeue()
    currentToken()
  }

  override def fieldName(): Option[String] = if(nodes.isEmpty) None else {
    val node = nodes.front
    if(node.token.isFieldName) node.string
    else None
  }

  override def string(): Option[String] = if(nodes.isEmpty) None else nodes.front.string

  override def number(): Option[Number] = if(nodes.isEmpty) None else nodes.front.number

  override def boolean(): Option[Boolean] = if(nodes.isEmpty) None else nodes.front.boolean
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
