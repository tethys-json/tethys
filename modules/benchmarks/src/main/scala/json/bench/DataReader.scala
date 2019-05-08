package json.bench

import json.bench.circe.CirceBench
import json.bench.handwritten.HandwrittenBench
import json.bench.json4s.Json4sBench
import json.bench.tethysjson.TethysBench
import json.bench.model.Data
import json.bench.play.PlayBench
import json.bench.pushka.PushkaBench
import json.bench.spray.SprayBench

trait DataReader {
  def read(json: String): Seq[Data]
}

object DataReader {
  val instances: Map[String, DataReader] = Map[String, DataReader](
    "tethys-jackson" -> TethysBench.TethysJacksonDataProcessor,
    "pure-jackson" -> HandwrittenBench.HandwrittenJacksonDataProcessor,
    "circe-jawn" -> CirceBench.CirceJawnDataReader,
    "circe-jackson" -> CirceBench.CirceJacksonDataReader,
    "json4s-jackson" -> Json4sBench.Json4sJacksonDataProcessor,
    "json4s-native" -> Json4sBench.Json4sNativeDataProcessor,
    "play-json" -> PlayBench.PlayDataProcessor,
    "spray-json" -> SprayBench.SprayDataProcessor,
    "pushka" -> PushkaBench.PushkaDataProcessor
  )
}
