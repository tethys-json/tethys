package json.bench.play

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import play.api.libs.json._

object PlayBench {

  implicit val dataWrites: Writes[Data] = Json.writes[Data]
  implicit val dataReads: Reads[Data] = Json.reads[Data]

  object PlayDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String =
      Json.stringify(Json.toJson(seq))

    override def read(json: String): Seq[Data] = Json.parse(json).as[Seq[Data]]
  }

}
