package json.bench.jsoniter

import json.bench.model.Data

import com.github.plokhotnyuk.jsoniter_scala.macros._
import com.github.plokhotnyuk.jsoniter_scala.core._
import json.bench.{DataReader, DataWriter}

object JsoniterBench {
  implicit val dataWrites: JsonValueCodec[Seq[Data]] = JsonCodecMaker.make

  object JsoniterProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = writeToString(seq)

    override def read(json: String): Seq[Data] = readFromString[Seq[Data]](json)
  }
}
