package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.instances.SimpleJsonReader.FieldDefinition
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

import scala.annotation.tailrec
import scala.collection.mutable

private[readers] class SimpleJsonReader[A](fields: Array[FieldDefinition[_]], mapper: Array[Any] => A, strict: Boolean) extends JsonReader[A] {

  private val defaults: Array[Any] = fields.map(_.defaultValue)

  override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
    if(!it.currentToken().isObjectStart) ReaderError.wrongJson(s"Expected object start but found: ${it.currentToken()}")
    else {
      it.nextToken()
      val extracted: Array[Any] = recRead(it, defaults.clone())

      val notExtracted = collectNotExtracted(0, hasErrors = false, extracted, new mutable.StringBuilder())
      if(notExtracted.nonEmpty) ReaderError.wrongJson(s"Can not extract fields $notExtracted")
      else mapper(extracted)
    }
  }

  @tailrec
  private def collectNotExtracted(i: Int, hasErrors: Boolean, extracted: Array[Any], builder: mutable.StringBuilder): String = {
    if(i >= extracted.length) {
      if(hasErrors) builder.append('\'').result()
      else ""
    } else if(extracted(i) == null) {
      if(!hasErrors) collectNotExtracted(i + 1, hasErrors = true, extracted, builder.append('\'').append(fields(i).name))
      else collectNotExtracted(i + 1, hasErrors = true, extracted, builder.append("', '").append(fields(i).name))
    } else {
      collectNotExtracted(i + 1, hasErrors, extracted, builder)
    }
  }

  @tailrec
  private def recRead(it: TokenIterator, extracted: Array[Any])(implicit fieldName: FieldName): Array[Any] = {
    it.currentToken() match {
      case token if token.isObjectEnd =>
        it.nextToken()
        extracted
      case token if token.isFieldName =>
        val name = it.fieldName()
        recRead(it, extractField(0, name, it.next(), extracted))
      case token =>
        ReaderError.wrongJson(s"Expect end of object or field name but '$token' found")
    }
  }

  @tailrec
  private def extractField(i: Int,
                           name: String,
                           it: TokenIterator,
                           extracted: Array[Any])
                          (implicit
                           fieldName: FieldName): Array[Any] = {
    if(i >= extracted.length) {
      if(strict) {
        ReaderError.wrongJson(s"unexpected field '$name', expected one of ${fields.map(_.name).mkString("'", "', '", "'")}")
      } else {
        it.skipExpression()
        extracted
      }
    } else {
      val field = fields(i)
      if(field.name == name) {
        extracted(i) = field.reader.read(it)(fieldName.appendFieldName(name))
        extracted
      } else {
        extractField(i + 1, name, it, extracted)
      }
    }
  }
}

private[readers] object SimpleJsonReader {
  case class FieldDefinition[A](name: String, defaultValue: Any, reader: JsonReader[A])
}