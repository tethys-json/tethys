package tethys.enumeratum

import _root_.enumeratum.{Enum, EnumEntry}
import tethys.{JsonReader, JsonWriter}

trait TethysEnum[A <: EnumEntry] { _: Enum[A] =>
  implicit val tethysReader: JsonReader[A] = Enumeratum.reader(this)
  implicit val tethysWriter: JsonWriter[A] = Enumeratum.writer(this)
}