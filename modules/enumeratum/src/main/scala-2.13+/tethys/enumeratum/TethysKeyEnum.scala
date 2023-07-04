package tethys.enumeratum

import _root_.enumeratum.{Enum, EnumEntry}
import tethys.readers.KeyReader
import tethys.writers.KeyWriter

trait TethysKeyEnum[A <: EnumEntry] { _: Enum[A] =>
  implicit val tethysKeyReader: KeyReader[A] = Enumeratum.keyReader(this)(_.withNameOption)
  implicit val tethysKeyWriter: KeyWriter[A] = Enumeratum.keyWriter(_.entryName)
}