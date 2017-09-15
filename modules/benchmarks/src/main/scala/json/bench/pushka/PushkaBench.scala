package json.bench.pushka

import json.bench.model.Data
import json.bench.{DataReader, DataWriter}
import pushka.json._
import json.bench.model.Data.bigDecimalRW

object PushkaBench {

  object PushkaDataProcessor extends DataWriter with DataReader {
    override def write(seq: Seq[Data]): String = pushka.json.write(seq)

    override def read(json: String): Seq[Data] = pushka.json.read[Seq[Data]](json)
  }

}
