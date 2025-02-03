package json.bench.tethysjson

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import tethys.*, tethys.given


/** Created by eld0727 on 21.04.17.
  */
object TethysBench {

  given JsonWriter[Seq[Data]] = JsonWriter.iterableWriter(JsonWriter.derived[Data])
  given JsonReader[Seq[Data]] = JsonReader.iterableReader(JsonReader.derived[Data])

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
