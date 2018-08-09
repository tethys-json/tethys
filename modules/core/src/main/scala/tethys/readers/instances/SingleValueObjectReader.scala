package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

import scala.annotation.tailrec
import scala.reflect.ClassTag

class SingleValueObjectReader[A: ClassTag](name: String, reader: JsonReader[A]) extends JsonReader[A] {


  override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
    if(!it.currentToken().isObjectStart) ReaderError.wrongType[A]
    else readField(it)
  }

  @tailrec
  private def readField(it: TokenIterator)(implicit fieldName: FieldName): A = {
    it.currentToken() match {
      case token if token.isObjectEnd =>
        it.nextToken()
        reader.defaultValue.getOrElse(ReaderError.wrongJson(s"'$name' field is missing"))
      case token if token.isFieldName =>
        val currentName = it.fieldName()
        it.nextToken()
        if(currentName == name) {
          val res = reader.read(it)(fieldName.appendFieldName(name))
          skipRest(it)
          res
        } else {
          readField(it)
        }

      case token => ReaderError.wrongJson(s"Expect end of object or field name but '$token' found")
    }
  }

  @tailrec
  private def skipRest(it: TokenIterator)(implicit fieldName: FieldName): Unit = {
    it.currentToken() match {
      case token if token.isObjectEnd =>
        it.nextToken()

      case token if token.isFieldName =>
        it.nextToken()
        it.skipExpression()
        skipRest(it)

      case token =>
        ReaderError.wrongJson(s"Expect end of object or field name but '$token' found")
    }
  }
}
