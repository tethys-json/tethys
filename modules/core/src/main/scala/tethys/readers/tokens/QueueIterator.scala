package tethys.readers.tokens

import tethys.commons.TokenNode._
import tethys.commons.{Token, TokenNode}
import tethys.readers.tokens.QueueIterator.WrongTokenError
import tethys.readers.tokens.TokenIterator.CopySupport

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.ClassTag

class QueueIterator(private var nodes: immutable.Queue[TokenNode])
    extends BaseTokenIterator
    with CopySupport {

  override def copy(): QueueIterator = QueueIterator(nodes)

  override def currentToken(): Token =
    if (nodes.isEmpty) Token.Empty else nodes.head.token

  override def nextToken(): Token = if (nodes.isEmpty) Token.Empty
  else {
    nodes = nodes.tail
    currentToken()
  }

  override def fieldName(): String = nodes.front match {
    case FieldNameNode(name) => name
    case node                => fail[FieldNameNode](node)
  }

  override def string(): String = nodes.front match {
    case StringValueNode(value) => value
    case node                   => fail[StringValueNode](node)
  }

  override def boolean(): Boolean = nodes.front match {
    case BooleanValueNode(value) => value
    case node                    => fail[BooleanValueNode](node)
  }

  override def number(): Number = nodes.front match {
    case NumberValueNode(value) => value
    case ShortValueNode(value)  => value
    case IntValueNode(value)    => value
    case LongValueNode(value)   => value
    case FloatValueNode(value)  => value
    case DoubleValueNode(value) => value
    case node                   => fail[NumberValueNode](node)
  }

  override def short(): Short = nodes.front match {
    case ShortValueNode(value)  => value
    case NumberValueNode(value) => value.shortValue()
    case IntValueNode(value)    => value.toShort
    case LongValueNode(value)   => value.toShort
    case FloatValueNode(value)  => value.toShort
    case DoubleValueNode(value) => value.toShort
    case node                   => fail[ShortValueNode](node)
  }

  override def int(): Int = nodes.front match {
    case IntValueNode(value)    => value
    case NumberValueNode(value) => value.intValue()
    case ShortValueNode(value)  => value.toInt
    case LongValueNode(value)   => value.toInt
    case FloatValueNode(value)  => value.toInt
    case DoubleValueNode(value) => value.toInt
    case node                   => fail[IntValueNode](node)
  }

  override def long(): Long = nodes.front match {
    case LongValueNode(value)   => value
    case NumberValueNode(value) => value.longValue()
    case ShortValueNode(value)  => value.toLong
    case IntValueNode(value)    => value.toLong
    case FloatValueNode(value)  => value.toLong
    case DoubleValueNode(value) => value.toLong
    case node                   => fail[LongValueNode](node)
  }

  override def float(): Float = nodes.front match {
    case FloatValueNode(value)  => value
    case NumberValueNode(value) => value.floatValue()
    case ShortValueNode(value)  => value.toFloat
    case IntValueNode(value)    => value.toFloat
    case LongValueNode(value)   => value.toFloat
    case DoubleValueNode(value) => value.toFloat
    case node                   => fail[FloatValueNode](node)
  }

  override def double(): Double = nodes.front match {
    case DoubleValueNode(value) => value
    case NumberValueNode(value) => value.doubleValue()
    case ShortValueNode(value)  => value.toDouble
    case IntValueNode(value)    => value.toDouble
    case LongValueNode(value)   => value.toDouble
    case FloatValueNode(value)  => value.toDouble
    case node                   => fail[DoubleValueNode](node)
  }

  override def collectExpression(): TokenIterator with CopySupport = {
    val node = currentNode()
    val queue = getTokenShift(node) match {
      case 0 => immutable.Queue[TokenNode](node)
      case _ => collectTokens(1, immutable.Queue.newBuilder[TokenNode] += node)
    }

    new QueueIterator(queue)
  }

  @tailrec
  private def collectTokens(
      started: Int,
      builder: mutable.Builder[TokenNode, immutable.Queue[TokenNode]]
  ): immutable.Queue[TokenNode] = {
    if (started == 0) builder.result()
    else {
      val node = currentNode()
      collectTokens(started + getTokenShift(node), builder += node)
    }
  }

  private def currentNode(): TokenNode = {
    if (nodes.isEmpty)
      throw new NoSuchElementException("Can not finish expression")
    else {
      val (head, tail) = nodes.dequeue
      nodes = tail
      head
    }
  }

  private def getTokenShift(node: TokenNode): Int = node match {
    case ArrayStartNode | ObjectStartNode => 1
    case ArrayEndNode | ObjectEndNode     => -1
    case _                                => 0
  }

  private def fail[A <: TokenNode](
      actual: TokenNode
  )(implicit ct: ClassTag[A]): Nothing = {
    throw new WrongTokenError(
      s"Expected '${ct.toString()}' but '$actual' found"
    )
  }
}

object QueueIterator {
  def apply(nodes: Seq[TokenNode]): QueueIterator = new QueueIterator(
    immutable.Queue[TokenNode](nodes: _*)
  )

  final class WrongTokenError(message: String) extends Exception(message, null)
}
