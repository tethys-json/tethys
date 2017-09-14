package tethys.core.readers.tokens

import tethys.core.readers.tokens.QueueIterator.TokenNode

import scala.annotation.tailrec
import scala.collection.mutable

trait BaseTokenIterator extends TokenIterator {
  override def next(): this.type = {
    nextToken()
    this
  }

  override def skipExpression(): this.type = {
    val token = currentToken()
    if(token.isStructStart) skipStructure(1).next()
    else next()
  }

  override def collectExpression(): TokenIterator = {
    val queue = createTokenNode() match {
      case (node, 0) => mutable.Queue[TokenNode](node)
      case (node, _) => collectTokens(1, mutable.Queue.newBuilder[TokenNode] += node)
    }

    nextToken()//set pointer after this expression end

    new QueueIterator(queue)
  }

  @tailrec
  private def skipStructure(started: Int): this.type = {
    if(started == 0) this
    else {
      val token = nextToken()
      if(token.isStructStart) skipStructure(started + 1)
      else if(token.isStructEnd) skipStructure(started - 1)
      else skipStructure(started)
    }
  }

  @tailrec
  private def collectTokens(started: Int, builder: mutable.Builder[TokenNode, mutable.Queue[TokenNode]]): mutable.Queue[TokenNode] = {
    if(started == 0) builder.result()
    else {
      nextToken()
      val (node, shift) = createTokenNode()
      collectTokens(started + shift, builder += node)
    }
  }

  private def createTokenNode(): (TokenNode, Int) = {
    val token = currentToken()
    if(token.isStructStart) TokenNode(token) -> 1
    else if(token.isStructEnd) TokenNode(token) -> -1
    else if(token.isNullValue) TokenNode(token) -> 0
    else if(token.isFieldName) TokenNode(token, string = fieldName()) -> 0
    else if(token.isStringValue) TokenNode(token, string = string()) -> 0
    else if(token.isNumberValue) TokenNode(token, number = number()) -> 0
    else TokenNode(token, boolean = boolean()) -> 0
  }
}
