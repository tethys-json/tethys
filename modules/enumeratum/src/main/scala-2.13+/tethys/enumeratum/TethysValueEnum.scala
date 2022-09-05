package tethys.enumeratum

import _root_.enumeratum.values._
import tethys.readers.KeyReader
import tethys.writers.KeyWriter
import tethys.{JsonReader, JsonWriter}

sealed trait TethysValueEnum[ValueType, EntryType <: ValueEnumEntry[ValueType]] { _: ValueEnum[ValueType, EntryType] =>
  implicit def tethysReader: JsonReader[EntryType]
  implicit def tethysWriter: JsonWriter[EntryType]
}

trait StringTethysEnum[E <: StringEnumEntry] extends TethysValueEnum[String, E] { _: ValueEnum[String, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
  implicit val tethysKeyReader: KeyReader[E] = Enumeratum.keyReader(this)(_.withValueOpt)
  implicit val tethysKeyWriter: KeyWriter[E] = Enumeratum.keyWriter(_.value)
}

trait IntTethysEnum[E <: IntEnumEntry] extends TethysValueEnum[Int, E] { _: ValueEnum[Int, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
}

trait LongTethysEnum[E <: LongEnumEntry] extends TethysValueEnum[Long, E] { _: ValueEnum[Long, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
}

trait ShortTethysEnum[E <: ShortEnumEntry] extends TethysValueEnum[Short, E] { _: ValueEnum[Short, E] =>
  implicit val tethysReader: JsonReader[E] = Enumeratum.valueReader(this)
  implicit val tethysWriter: JsonWriter[E] = Enumeratum.valueWriter(this)
}