package json.bench.tethysjson

import json.bench.{DataReader, DataWriter}
import json.bench.model.Data
import tethys._
import tethys.jackson._

object TethysBench {

  implicit val dataWriter: JsonWriter[Data] =
    tethys.derivation.semiauto.jsonWriter[Data]
  implicit val dataReader: JsonReader[Data] =
    tethys.derivation.semiauto.jsonReader[Data]

  object TethysDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = seq.asJson
    override def read(json: String): Seq[Data] = ???
  }

  object TethysJacksonDataProcessor extends DataWriter with DataReader {

    override def write(seq: Seq[Data]): String = seq.asJson

    override def read(json: String): Seq[Data] =
      json.jsonAs[Seq[Data]].toOption.get
  }
}
