package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.instances.SimpleJsonReader.FieldDefinition
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

import scala.annotation.tailrec
import scala.collection.mutable

private[readers] class SimpleJsonReader[A](
    fields: Array[FieldDefinition[_]],
    mapper: Array[Any] => A,
    strict: Boolean
) extends JsonReader[A] {

  override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
    if (!it.currentToken().isObjectStart)
      ReaderError.wrongJson(
        s"Expected object start but found: ${it.currentToken()}"
      )
    else {
      it.nextToken()
      val extracted: Map[String, Any] = recRead(it, Map())
      val extractedWithDefaultValues = addDefaultValues(0, extracted)
      val notExtracted = collectNotExtracted(
        0,
        hasErrors = false,
        extractedWithDefaultValues.keySet,
        new mutable.StringBuilder()
      )
      if (notExtracted.nonEmpty)
        ReaderError.wrongJson(s"Can not extract fields $notExtracted")
      else {
        val arr = fields.map(f => extractedWithDefaultValues(f.name))
        mapper(arr)
      }
    }
  }

  override def defaultValue: Option[A] = None

  @tailrec
  private def collectNotExtracted(
      i: Int,
      hasErrors: Boolean,
      extracted: Set[String],
      builder: mutable.StringBuilder
  ): String = {
    if (i >= fields.length) {
      if (hasErrors) builder.append('\'').result()
      else ""
    } else {
      val name = fields(i).name
      if (extracted.contains(name)) {
        collectNotExtracted(i + 1, hasErrors, extracted, builder)
      } else {
        if (!hasErrors)
          collectNotExtracted(
            i + 1,
            hasErrors = true,
            extracted,
            builder.append('\'').append(name)
          )
        else
          collectNotExtracted(
            i + 1,
            hasErrors = true,
            extracted,
            builder.append("', '").append(name)
          )
      }
    }
  }

  @tailrec
  private def addDefaultValues(
      i: Int,
      extracted: Map[String, Any]
  ): Map[String, Any] = {
    if (i >= fields.length) {
      extracted
    } else {
      val name = fields(i).name
      addDefaultValues(
        i + 1,
        if (extracted.contains(name)) {
          extracted
        } else {
          try {
            fields(i).reader.defaultValue
              .map(
                extracted.updated(name, _)
              )
              .getOrElse(extracted)
          } catch {
            case e: IllegalArgumentException => extracted
          }
        }
      )
    }
  }

  @tailrec
  private def recRead(it: TokenIterator, extracted: Map[String, Any])(implicit
      fieldName: FieldName
  ): Map[String, Any] = {
    it.currentToken() match {
      case token if token.isObjectEnd =>
        it.nextToken()
        extracted
      case token if token.isFieldName =>
        val name = it.fieldName()
        recRead(it, extractField(0, name, it.next(), extracted))
      case token =>
        ReaderError.wrongJson(
          s"Expect end of object or field name but '$token' found"
        )
    }
  }

  @tailrec
  private def extractField(
      i: Int,
      name: String,
      it: TokenIterator,
      extracted: Map[String, Any]
  )(implicit fieldName: FieldName): Map[String, Any] = {
    if (i >= fields.size) {
      if (strict) {
        ReaderError.wrongJson(
          s"unexpected field '$name', expected one of ${fields.map(_.name).mkString("'", "', '", "'")}"
        )
      } else {
        it.skipExpression()
        extracted
      }
    } else {
      val field = fields(i)
      if (field.name == name) {
        extracted.updated(
          name,
          field.reader.read(it)(fieldName.appendFieldName(name))
        )
      } else {
        extractField(i + 1, name, it, extracted)
      }
    }
  }
}

private[readers] object SimpleJsonReader {
  case class FieldDefinition[A](
      name: String,
      reader: JsonReader[A]
  )
}
