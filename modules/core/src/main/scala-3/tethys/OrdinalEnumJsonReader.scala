package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

class OrdinalEnumJsonReader[A <: scala.reflect.Enum](getByOrdinal: Int => A)
    extends JsonReader[A]:
  override def read(it: TokenIterator)(using FieldName): A =
    if it.currentToken().isNumberValue then
      val res = it.int()
      it.next()
      try getByOrdinal(res)
      catch
        case ex: NoSuchElementException =>
          ReaderError.wrongJson(s"Unknown enum ordinal: $res")
    else
      ReaderError.wrongJson(
        s"Expected int value but found: ${it.currentToken()}"
      )

object OrdinalEnumJsonReader:
  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumJsonReader[A] =
    new OrdinalEnumJsonReader[A](derivation.EnumCompanion.getByOrdinal[A])
