package json.bench.spray

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import spray.json.DefaultJsonProtocol._
import spray.json._

object SprayBench {

  implicit val dataFormat: RootJsonFormat[Data] = jsonFormat6(Data.apply)

  object SprayDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = seq.toJson.compactPrint

    override def read(json: String): Seq[Data] =
      json.parseJson.convertTo[Seq[Data]]
  }

}
