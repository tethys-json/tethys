package tethys.fs2

import tethys.JsonReader
import tethys.commons.Token
import tethys.commons.TokenNode
import tethys.commons.TokenNode._
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.QueueIterator

import scala.collection.mutable

private[tethys] object JsonStreamSupport {

  final class Collector private () {
    private[this] val nodes: mutable.ArrayBuffer[TokenNode] =
      mutable.ArrayBuffer.empty
    private[this] var openStructures: Int = 0
    private[this] var complete: Boolean = false

    def consume(cursor: Cursor): Unit = {
      val token = cursor.currentToken()
      nodes += toNode(token, cursor)
      openStructures += tokenShift(token)
      complete = openStructures == 0
    }

    def isComplete: Boolean = complete

    def tokens: List[TokenNode] = nodes.toList
  }

  object Collector {
    def start(cursor: Cursor): Collector = {
      val collector = new Collector()
      collector.consume(cursor)
      collector
    }
  }

  def decode[A](
      collector: Collector,
      jsonReader: JsonReader[A]
  ): Either[ReaderError, A] = {
    implicit val fieldName: FieldName = FieldName()
    ReaderError.catchNonFatal {
      jsonReader.read(QueueIterator(collector.tokens))
    }
  }

  def rootError(reason: String): ReaderError = {
    implicit val fieldName: FieldName = FieldName()
    try ReaderError.wrongJson(reason)
    catch {
      case error: ReaderError => error
    }
  }

  def matchesArrayPath(cursor: Cursor, path: List[String]): Boolean = {
    cursor.currentToken().isArrayStart && {
      if (path.isEmpty) {
        cursor.scopeDepth == 1 && cursor.currentName.isEmpty
      } else {
        cursor.currentName.contains(path.last) && cursor.history == path
      }
    }
  }

  def isImmediateArrayElementStart(
      cursor: Cursor,
      targetArrayDepth: Int
  ): Boolean = {
    val token = cursor.currentToken()

    if (
      token.isFieldName || token.isArrayEnd || token.isObjectEnd || token.isEmpty
    ) {
      false
    } else if (token.isArrayStart || token.isObjectStart) {
      cursor.scopeDepth == targetArrayDepth + 1
    } else {
      cursor.scopeDepth == targetArrayDepth
    }
  }

  private def tokenShift(token: Token): Int = {
    if (token.isArrayStart || token.isObjectStart) 1
    else if (token.isArrayEnd || token.isObjectEnd) -1
    else 0
  }

  private def toNode(token: Token, cursor: Cursor): TokenNode = {
    if (token.isArrayStart) ArrayStartNode
    else if (token.isArrayEnd) ArrayEndNode
    else if (token.isObjectStart) ObjectStartNode
    else if (token.isObjectEnd) ObjectEndNode
    else if (token.isNullValue) NullValueNode
    else if (token.isFieldName) FieldNameNode(cursor.fieldName())
    else if (token.isStringValue) StringValueNode(cursor.string())
    else if (token.isNumberValue) cursor.number() match {
      case v: java.lang.Byte    => ByteValueNode(v)
      case v: java.lang.Short   => ShortValueNode(v)
      case v: java.lang.Integer => IntValueNode(v)
      case v: java.lang.Long    => LongValueNode(v)
      case v: java.lang.Float   => FloatValueNode(v)
      case v: java.lang.Double  => DoubleValueNode(v)
      case n                    => NumberValueNode(n)
    }
    else if (token.isBooleanValue) BooleanValueNode(cursor.boolean())
    else {
      throw new IllegalStateException(
        s"Unsupported token node conversion for token: $token"
      )
    }
  }
}
