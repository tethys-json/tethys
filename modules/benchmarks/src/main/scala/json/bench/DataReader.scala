package json.bench

import json.bench.circe.CirceBench
import json.bench.handwritten.HandwrittenBench
import json.bench.json4s.Json4sBench
import json.bench.jsoniter.JsoniterBench
import json.bench.model.Data
import json.bench.play.PlayBench
import json.bench.spray.SprayBench
import json.bench.tethysjson.TethysBench
import json.bench.ziojson.ZIOJsonBench

trait DataReader {
  def read(json: String): Seq[Data]
}

object DataReader {
  val instances: Map[String, DataReader] = Map[String, DataReader](
    "tethys-jackson" -> TethysBench.TethysJacksonDataProcessor,
    "tethys" -> TethysBench.TethysDataProcessor,
    "pure-jackson" -> HandwrittenBench.HandwrittenJacksonDataProcessor,
    "circe-jawn" -> CirceBench.CirceJawnDataReader,
    "circe-jackson" -> CirceBench.CirceJacksonDataReader,
    "jsoniter" -> JsoniterBench.JsoniterProcessor,
    "json4s-jackson" -> Json4sBench.Json4sJacksonDataProcessor,
    "json4s-native" -> Json4sBench.Json4sNativeDataProcessor,
    "play-json" -> PlayBench.PlayDataProcessor,
    "spray-json" -> SprayBench.SprayDataProcessor,
    "zio-json" -> ZIOJsonBench.ZIOJsonDataProcesser
  )
}
