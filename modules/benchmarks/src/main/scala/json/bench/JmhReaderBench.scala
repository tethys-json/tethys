package json.bench

import java.util.concurrent.TimeUnit

import json.bench.model.Data
import json.bench.tethysjson.TethysBench.TethysJacksonDataProcessor
import org.openjdk.jmh.annotations.{State, _}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 5, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms1G", "-Xmx1G"))
@State(Scope.Benchmark)
class JmhReaderBench {
  @Param(
    Array(
      "128b",
      "1kb",
      "128kb",
      "1mb",
      "32mb"
    )
  )
  var jsonSize: String = _

  val seed = 10000

  var data: String = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val entities = jsonSize match {
      case "128b"  => Data.dataSamples(1, seed)
      case "1kb"   => Data.dataSamples(8, seed)
      case "128kb" => Data.dataSamples(128 * 8, seed)
      case "1mb"   => Data.dataSamples(8 * 128 * 8, seed)
      case "32mb"  => Data.dataSamples(32 * 8 * 128 * 8, seed)
    }
    data = TethysJacksonDataProcessor.write(entities)
  }

  @Param(
    Array(
      "tethys-jackson",
      "pure-jackson",
      "circe-jawn",
      "circe-jackson",
      "play-json",
      "spray-json",
      "zio-json"
    )
  )
  var processorName: String = _

  @Benchmark
  def bench: Seq[Data] = {
    DataReader.instances(processorName).read(data)
  }
}
