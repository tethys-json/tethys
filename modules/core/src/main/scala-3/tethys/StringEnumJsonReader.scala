package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

trait StringEnumJsonReader[A] extends JsonReader[A]:
  // override def defaultValue: A = null.asInstanceOf[A]
  override def defaultValue: A = throw new IllegalArgumentException("StringEnumJsonReader does not have default value")

object StringEnumJsonReader:
  def from[A](impl: TokenIterator => FieldName ?=> A): StringEnumJsonReader[A] =
    new StringEnumJsonReader[A]:
      def read(it: TokenIterator)(implicit fieldName: FieldName): A = impl(it)

  inline def derived[A <: scala.reflect.Enum]: StringEnumJsonReader[A] =
    StringEnumJsonReader.from[A]: it =>
      if it.currentToken().isStringValue then
        val res = it.string()
        it.next()
        try derivation.EnumCompanion.getByName[A](res)
        catch
          case ex: NoSuchElementException =>
            ReaderError.wrongJson(s"Unknown enum name: $res")
      else
        ReaderError.wrongJson(
          s"Expected string value but found: ${it.currentToken()}"
        )
