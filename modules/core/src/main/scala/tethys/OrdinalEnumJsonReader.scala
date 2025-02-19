package tethys

import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

trait OrdinalEnumJsonReader[A] extends JsonReader[A]

object OrdinalEnumJsonReader:
  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumJsonReader[A] =
    new OrdinalEnumJsonReader[A]:
      def read(it: TokenIterator)(implicit fieldName: FieldName): A =
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
