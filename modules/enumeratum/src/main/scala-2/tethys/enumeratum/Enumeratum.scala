package tethys.enumeratum

import _root_.enumeratum._
import _root_.enumeratum.values._
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}
import tethys.writers.KeyWriter
import tethys.{JsonReader, JsonWriter}

object Enumeratum {

  def reader[A <: EnumEntry](enum: Enum[A]): JsonReader[A] = new JsonReader[A] {
    def read(it: TokenIterator)(implicit fieldName: FieldName): A =
      decode(enum)(_.withNameOption, JsonReader.stringReader.read(it))
  }

  def writer[A <: EnumEntry](enum: Enum[A]): JsonWriter[A] = JsonWriter.stringWriter.contramap[A](_.entryName)

  def keyReader[E, A](enum: E)(fn: E => String => Option[A]): KeyReader[A] = new KeyReader[A] {
    def read(str: String)(implicit fieldName: FieldName): A = decode(enum)(fn, str)
  }

  def keyWriter[A](fn: A => String): KeyWriter[A] = new KeyWriter[A] {
    def toKey(value: A): String = fn(value)
  }

  def valueReader[ValueType: JsonReader, EntryType <: ValueEnumEntry[ValueType]](
      enum: ValueEnum[ValueType, EntryType]
  ): JsonReader[EntryType] = new JsonReader[EntryType] {
    def read(it: TokenIterator)(implicit fieldName: FieldName): EntryType =
      decode(enum)(_.withValueOpt, JsonReader[ValueType].read(it))
  }

  def valueWriter[ValueType: JsonWriter, EntryType <: ValueEnumEntry[ValueType]](
      enum: ValueEnum[ValueType, EntryType]
  ): JsonWriter[EntryType] = JsonWriter[ValueType].contramap[EntryType](_.value)

  def decode[E, A, V](enum: E)(fn: E => V => Option[A], value: V)(implicit fieldName: FieldName): A =
    fn(enum)(value) match {
      case Some(result) => result
      case _            => ReaderError.wrongJson(s"$value is not a member of enum $enum")
    }
}
