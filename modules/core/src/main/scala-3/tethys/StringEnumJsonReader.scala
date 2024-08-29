package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

trait StringEnumJsonReader[A] extends JsonReader[A]

object StringEnumJsonReader:
  inline def derived[A <: scala.reflect.Enum]: StringEnumJsonReader[A] =
    new StringEnumJsonReader[A]:
      def read(it: TokenIterator)(implicit fieldName: FieldName): A =
        if it.currentToken().isStringValue then
          val res = it.string()
          it.next()
          try
            derivation.EnumCompanion.getByName[A](res)
          catch 
            case ex: NoSuchElementException =>
              ReaderError.wrongJson(s"Unknown enum name: $res")
        else
          ReaderError.wrongJson(s"Expected string value but found: ${it.currentToken()}")
