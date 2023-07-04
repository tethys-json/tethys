package tethys.enumeratum

import tethys.JsonReader

case class Data(direction: Direction)
object Data {
  implicit val reader: JsonReader[Data] = JsonReader.builder
    .addField[Direction]("direction")
    .buildReader(Data.apply)
}
