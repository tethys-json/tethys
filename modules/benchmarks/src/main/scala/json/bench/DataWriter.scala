package json.bench

import json.bench.circe.CirceBench
import json.bench.handwritten.HandwrittenBench
import json.bench.json4s.Json4sBench
import json.bench.tethysjson.TethysBench
import json.bench.model.Data
import json.bench.play.PlayBench
import json.bench.pushka.PushkaBench
import json.bench.spray.SprayBench

trait DataWriter {
  def write(seq: Seq[Data]): String
}

object DataWriter {
  val instances: Map[String, DataWriter] = Map(
    "tethys-jackson" -> TethysBench.TethysJacksonDataProcessor,
    "pure-jackson" -> HandwrittenBench.HandwrittenJacksonDataProcessor,
    "circe" -> CirceBench.CirceDataWriter,
    "java.lang.StringBuilder" -> HandwrittenBench.HandwrittenJavaDataWriter,
    "scala.StringBuilder" -> HandwrittenBench.HandwrittenScalaDataWriter,
    "json4s-jackson" -> Json4sBench.Json4sJacksonDataProcessor,
    "json4s-native" -> Json4sBench.Json4sNativeDataProcessor,
    "play-json" -> PlayBench.PlayDataProcessor,
    "spray-json" -> SprayBench.SprayDataProcessor,
    "pushka" -> PushkaBench.PushkaDataProcessor
  )
}
