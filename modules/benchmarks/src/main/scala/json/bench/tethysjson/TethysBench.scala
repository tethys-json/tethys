package json.bench.tethysjson

import json.bench.model.Data
import tethys._
import tethys.jackson._
import tethys.derivation.semiauto._
import json.bench.{DataReader, DataWriter}

/**
  * Created by eld0727 on 21.04.17.
  */
object TethysBench {

  implicit val dataWriter: JsonWriter[Data] = jsonWriter[Data]
  implicit val dataReader: JsonReader[Data] = jsonReader[Data]

  object TethysJacksonDataProcessor extends DataWriter with DataReader {

    override def write(seq: Seq[Data]): String = seq.asJson

    override def read(json: String): Seq[Data] = json.jsonAs[Seq[Data]].toOption.get
  }
}
