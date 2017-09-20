package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.annotation.tailrec
import scala.reflect.ClassTag

class SimpleJsonReader[A: ClassTag](fields: Map[String, JsonReader[_]])(mapper: Map[String, Any] => A) extends JsonReader[A] {

  private val names: List[String] = fields.keys.toList
  private val defaults: Map[String, Any] = fields.flatMap {
    case (name, jsonReader) => jsonReader.defaultValue.map(name -> _)
  }

  override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
    if(!it.currentToken().isObjectStart) ReaderError.wrongType[A]
    else {
      it.nextToken()
      val extracted: Map[String, Any] = recRead(it, defaults)

      if(!names.forall(extracted.contains)) ReaderError.wrongJson
      else mapper(extracted)
    }
  }

  @tailrec
  private def recRead(it: TokenIterator, extracted: Map[String, Any])(implicit fieldName: FieldName): Map[String, Any] = {
    it.currentToken() match {
      case token if token.isObjectEnd =>
        it.nextToken()
        extracted
      case token if token.isFieldName =>
        val name = it.fieldName()
        it.nextToken()
        fields.get(name) match {
          case Some(reader) =>
            val value = reader.read(it)(fieldName.appendFieldName(name))
            recRead(it, extracted.updated(name, value))

          case _ =>
            it.skipExpression()
            recRead(it, extracted)
        }
      case _ => ReaderError.wrongJson
    }
  }
}
