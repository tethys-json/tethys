package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

trait OrdinalEnumJsonReader[A] extends JsonReader[A]:
  override def defaultValue: Option[A] = None

object OrdinalEnumJsonReader:
  def from[A](
      impl: TokenIterator => FieldName ?=> A
  ): OrdinalEnumJsonReader[A] =
    new OrdinalEnumJsonReader[A]:
      def read(it: TokenIterator)(implicit fieldName: FieldName): A = impl(it)

  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumJsonReader[A] =
    OrdinalEnumJsonReader.from[A]: it =>
      if it.currentToken().isNumberValue then
        val res = it.int()
        it.next()
        try derivation.EnumCompanion.getByOrdinal[A](res)
        catch
          case ex: NoSuchElementException =>
            ReaderError.wrongJson(s"Unknown enum ordinal: $res")
      else
        ReaderError.wrongJson(
          s"Expected int value but found: ${it.currentToken()}"
        )
