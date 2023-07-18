package tethys.enumeratum

import enumeratum.{Enum, EnumEntry}

sealed trait Direction extends EnumEntry
case object Direction extends Enum[Direction] with TethysEnum[Direction] with TethysKeyEnum[Direction] {
  case object Up extends    Direction
  case object Down extends  Direction
  case object Left extends  Direction
  case object Right extends Direction

  val values = findValues
}