package json.bench.ziojson

import zio.json._
import json.bench._
import json.bench.model.Data

object ZIOJsonBench {
  implicit val decoder: JsonDecoder[Data] = DeriveJsonDecoder.gen[Data]
  implicit val encoder: JsonEncoder[Data] = DeriveJsonEncoder.gen[Data]

  object ZIOJsonDataProcesser extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String =
      seq.toJson

    override def read(json: String): Seq[Data] =
      json.fromJson[Seq[Data]].toOption.get
  }

}
