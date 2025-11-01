package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

trait StringEnumJsonReader[A] extends JsonReader[A]

object StringEnumJsonReader:
  def from[A](impl: TokenIterator => FieldName ?=> A): StringEnumJsonReader[A] =
    new StringEnumJsonReader[A]:
      def read(it: TokenIterator)(implicit fieldName: FieldName): A = impl(it)

  inline def derived[A <: scala.reflect.Enum](
      f: A => String
  ): StringEnumJsonReader[A] =
    val valuesMap =
      derivation.EnumCompanion.getValues[A].map(x => f(x) -> x).toMap
    StringEnumJsonReader.from[A](impl(valuesMap.apply))

  inline def derived[A <: scala.reflect.Enum]: StringEnumJsonReader[A] =
    StringEnumJsonReader.from[A](impl(derivation.EnumCompanion.getByName[A]))

  private def impl[A](get: String => A): TokenIterator => FieldName ?=> A =
    it =>
      if it.currentToken().isStringValue then
        val res = it.string()
        it.next()
        try get(res)
        catch
          case ex: NoSuchElementException =>
            ReaderError.wrongJson(s"Unknown enum name: $res")
      else
        ReaderError.wrongJson(
          s"Expected string value but found: ${it.currentToken()}"
        )
