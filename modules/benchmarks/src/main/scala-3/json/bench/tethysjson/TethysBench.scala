package json.bench.tethysjson

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import tethys._

/** Created by eld0727 on 21.04.17.
  */
object TethysBench {

  implicit val dataWriter: JsonWriter[Seq[Data]] =
    JsonWriter.iterableWriter(tethys.derivation.semiauto.jsonWriter[Data])
  implicit val dataReader: JsonReader[Seq[Data]] =
    JsonReader.iterableReader(tethys.derivation.semiauto.jsonReader[Data])

  object TethysDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = seq.asJson
    override def read(json: String): Seq[Data] = ???
  }

  object TethysJacksonDataProcessor extends DataWriter with DataReader {
    import tethys.jackson._

    override def write(seq: Seq[Data]): String = seq.asJson

    override def read(json: String): Seq[Data] =
      json.jsonAs[Seq[Data]].toOption.get
  }
}
