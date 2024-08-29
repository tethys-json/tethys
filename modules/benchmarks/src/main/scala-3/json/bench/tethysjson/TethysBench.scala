package json.bench.tethysjson

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import tethys.*
import tethys.jackson.*

/** Created by eld0727 on 21.04.17.
  */
object TethysBench {

  implicit val dataWriter: JsonWriter[Data] = JsonWriter.derived[Data]
  implicit val dataReader: JsonReader[Data] = JsonReader.derived[Data]

  object TethysJacksonDataProcessor extends DataWriter with DataReader {

    override def write(seq: Seq[Data]): String = seq.asJson

    override def read(json: String): Seq[Data] =
      json.jsonAs[Seq[Data]].toOption.get
  }
}
