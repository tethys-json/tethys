package tethys.readers.tokens

import tethys.commons.TokenNode
import tethys.commons.TokenNode._
import tethys.readers.tokens.TokenIterator.CopySupport

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable

trait BaseTokenIterator extends TokenIterator {
  override def next(): this.type = {
    nextToken()
    this
  }

  override def skipExpression(): this.type = {
    val token = currentToken()
    if (token.isStructStart) skipStructure(1).next()
    else next()
  }

  override def collectExpression(): TokenIterator with CopySupport = {
    val queue = createTokenNode() match {
      case (node, 0) => immutable.Queue[TokenNode](node)
      case (node, _) =>
        collectTokens(1, immutable.Queue.newBuilder[TokenNode] += node)
    }

    nextToken() // set pointer after this expression end

    new QueueIterator(queue)
  }

  @tailrec
  private def skipStructure(started: Int): this.type = {
    if (started == 0) this
    else {
      val token = nextToken()
      if (token.isStructStart) skipStructure(started + 1)
      else if (token.isStructEnd) skipStructure(started - 1)
      else skipStructure(started)
    }
  }

  @tailrec
  private def collectTokens(
      started: Int,
      builder: mutable.Builder[TokenNode, immutable.Queue[TokenNode]]
  ): immutable.Queue[TokenNode] = {
    if (started == 0) builder.result()
    else {
      nextToken()
      val (node, shift) = createTokenNode()
      collectTokens(started + shift, builder += node)
    }
  }

  private def createTokenNode(): (TokenNode, Int) = {
    val token = currentToken()
    if (token.isArrayStart) ArrayStartNode -> 1
    else if (token.isArrayEnd) ArrayEndNode -> -1
    else if (token.isObjectStart) ObjectStartNode -> 1
    else if (token.isObjectEnd) ObjectEndNode -> -1
    else if (token.isNullValue) NullValueNode -> 0
    else if (token.isFieldName) FieldNameNode(fieldName()) -> 0
    else if (token.isStringValue) StringValueNode(string()) -> 0
    else if (token.isNumberValue) number() match {
      case v: java.lang.Short   => ShortValueNode(v) -> 0
      case v: java.lang.Integer => IntValueNode(v) -> 0
      case v: java.lang.Long    => LongValueNode(v) -> 0
      case v: java.lang.Float   => FloatValueNode(v) -> 0
      case v: java.lang.Double  => DoubleValueNode(v) -> 0
      case n                    => NumberValueNode(n) -> 0
    }
    else BooleanValueNode(boolean()) -> 0
  }
}
