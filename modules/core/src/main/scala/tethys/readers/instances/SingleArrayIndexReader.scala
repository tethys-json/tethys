package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.annotation.tailrec

class SingleArrayIndexReader[A](index: Int, reader: JsonReader[A]) extends JsonReader[A] {
  override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
    if (it.currentToken().isArrayStart) recRead(0, it.next())
    else ReaderError.wrongJson(s"Expected array start, but '${it.currentToken()}' token found")
  }

  @tailrec
  private def recRead(i: Int, it: TokenIterator)
                     (implicit fieldName: FieldName): A = {
    it.currentToken() match {
      case token if token.isEmpty =>
        ReaderError.wrongJson("Unexpected end of input")
      case token if token.isArrayEnd =>
        it.nextToken()
        ReaderError.wrongJson(s"Looking for array element at $index but it has only $i elements")
      case _ =>
        if(i == index) {
          val res = reader.read(it)(fieldName.appendArrayIndex(i))
          recSkip(it)
          res
        } else {
          recRead(i + 1, it)
        }
    }
  }

  @tailrec
  private def recSkip(it: TokenIterator)
                     (implicit fieldName: FieldName): Unit = {
    it.currentToken() match {
      case token if token.isEmpty =>
        ReaderError.wrongJson("Unexpected end of input")
      case token if token.isArrayEnd =>
        it.nextToken()
      case _ =>
        it.skipExpression()
        recSkip(it)
    }
  }
}
