package json.bench.circe

import io.circe.generic.semiauto.*
import io.circe.*
import io.circe.syntax.*
import json.bench.model.Data
import json.bench.{DataReader, DataWriter}

object CirceBench {
  implicit val dataEncoder: Encoder[Data] = deriveEncoder[Data]
  implicit val dataDecoder: Decoder[Data] = deriveDecoder[Data]

  object CirceDataWriter extends DataWriter {
    override def write(seq: Seq[Data]): String = seq.asJson.noSpaces
  }

  object CirceJawnDataReader extends DataReader {
    override def read(json: String): Seq[Data] = jawn.decode[Seq[Data]](json).toOption.get
  }

  object CirceJacksonDataReader extends DataReader {
    override def read(json: String): Seq[Data] = jackson.decode[Seq[Data]](json).toOption.get
  }
}
