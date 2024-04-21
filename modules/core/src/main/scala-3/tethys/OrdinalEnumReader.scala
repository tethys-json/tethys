package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

trait OrdinalEnumReader[A] extends JsonReader[A]

object OrdinalEnumReader:
  inline def derived[A <: scala.reflect.Enum]: OrdinalEnumReader[A] =
    new OrdinalEnumReader[A]:
      def read(it: TokenIterator)(implicit fieldName: FieldName): A =
        if it.currentToken().isNumberValue then
          val res = it.int()
          it.next()
          derivation.EnumCompanion.getByOrdinal[A](res)
        else
          ReaderError.wrongJson(s"Expected int value but found: ${it.currentToken()}")

