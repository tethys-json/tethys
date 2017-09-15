package json.bench.circe

import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import json.bench.model.Data
import json.bench.{DataReader, DataWriter}

object CirceBench {

  implicit val dataEncoder: Encoder[Data] = deriveEncoder[Data]
  implicit val dataDecoder: Decoder[Data] = deriveDecoder[Data]

  object CirceDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = seq.asJson.noSpaces

    override def read(json: String): Seq[Data] = decode[Seq[Data]](json).right.get
  }
}
