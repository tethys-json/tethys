package json.bench.json4s

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import org.json4s._

object Json4sBench {
  implicit val format: DefaultFormats.type = DefaultFormats

  object Json4sNativeDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = org.json4s.native.Serialization.write(seq)

    override def read(json: String): Seq[Data] = org.json4s.native.parseJson(json).extract[Seq[Data]]
  }

  object Json4sJacksonDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = org.json4s.jackson.Serialization.write(seq)

    override def read(json: String): Seq[Data] = org.json4s.jackson.parseJson(json).extract[Seq[Data]]
  }
}
