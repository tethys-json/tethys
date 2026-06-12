package tethys.derivation

import scala.compiletime.summonFrom
import scala.deriving.Mirror

import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

object DerivationSupport:

  inline def summonOrDerivedReaderWith[T]: JsonReader[T]^ =
    summonFrom {
      case reader: JsonReader[T]^ => reader
      case given Mirror.Of[T] =>
        JsonReader.derivedWith[T]
    }

  inline def summonOrDerivedWriterWith[T]: JsonWriter[T]^ =
    summonFrom {
      case writer: JsonWriter[T]^ => writer
      case given Mirror.Of[T]     => JsonObjectWriter.derivedWith[T]
    }

  inline def summonOrDerivedObjectWriterWith[T]: JsonObjectWriter[T]^ =
    summonFrom {
      case writer: JsonObjectWriter[T]^ => writer
      case given Mirror.Of[T]           => JsonObjectWriter.derivedWith[T]
    }

  def widenReaderWith[T](reader: Any): JsonReader[T]^ =
    reader.asInstanceOf[JsonReader[T]^]

  inline def identityReaderWith[T](reader: JsonReader[T]^): JsonReader[T]^ = reader

  def readObjectFieldWith[A](
      it: TokenIterator^,
      label: String,
      reader: JsonReader[A]^
  )(using fieldName: FieldName): A =
    if !it.currentToken().isObjectStart then
      ReaderError.wrongJson(
        "Expected object start but found: " + it.currentToken().toString
      )

    it.nextToken()
    var result: Option[A] = None
    while !it.currentToken().isObjectEnd && result.isEmpty do
      val jsonName = it.fieldName()
      it.nextToken()
      if jsonName == label then
        result = Some(reader.read(it)(fieldName.appendFieldName(label)))
      else
        it.skipExpression()

    result.getOrElse {
      ReaderError.wrongJson(
        "Can not extract fields from json: '" + label + "'"
      )
    }
