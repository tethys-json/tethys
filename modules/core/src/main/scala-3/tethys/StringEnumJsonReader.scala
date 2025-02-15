package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

class StringEnumJsonReader[A <: scala.reflect.Enum](
    getByName: String => A
) extends JsonReader[A] {
  override def read(it: TokenIterator)(using FieldName): A =
    if it.currentToken().isStringValue then
      val res = it.string()
      it.next()
      try getByName(res)
      catch
        case ex: NoSuchElementException =>
          ReaderError.wrongJson(s"Unknown enum name: $res")
    else
      ReaderError.wrongJson(
        s"Expected string value but found: ${it.currentToken()}"
      )
}

object StringEnumJsonReader:
  inline def derived[A <: scala.reflect.Enum]: StringEnumJsonReader[A] =
    new StringEnumJsonReader[A](derivation.EnumCompanion.getByName[A])
