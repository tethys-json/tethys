package json.bench

import java.util.concurrent.TimeUnit

import json.bench.model.Data
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 4, time = 5, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 5, timeUnit = TimeUnit.SECONDS)
@Fork(value = 1, jvmArgsAppend = Array("-Xms1G", "-Xmx1G"))
@State(Scope.Benchmark)
class JmhWriterBench {
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
  var data: Seq[Data] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    data = jsonSize match {
      case "128b"  => Data.dataSamples(1, seed)
      case "1kb"   => Data.dataSamples(8, seed)
      case "128kb" => Data.dataSamples(128 * 8, seed)
      case "1mb"   => Data.dataSamples(8 * 128 * 8, seed)
      case "32mb"  => Data.dataSamples(32 * 8 * 128 * 8, seed)
    }
  }

  @Param(
    Array(
      "tethys-jackson",
      "pure-jackson",
      "circe",
      "java.lang.StringBuilder",
      "scala.StringBuilder",
      "json4s-jackson",
      "json4s-native",
      "play-json",
      "spray-json"
    )
  )
  var processorName: String = _

  @Benchmark
  def bench: String = {
    DataWriter.instances(processorName).write(data)
  }
}
