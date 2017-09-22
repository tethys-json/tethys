package json.bench

import java.util.concurrent.TimeUnit

import json.bench.model.Data
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 2, time = 30, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 30, timeUnit = TimeUnit.SECONDS)
@Fork(2)
@State(Scope.Thread)
class JmhWriterBench {
  @Param(Array(
    "10",
    "100",
    "1000",
    "10000",
    "100000"
  ))
  var arraySize: Int = _

  val seed = 10000
  var data: Seq[Data] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    data = Data.dataSamples(arraySize, seed)
  }

  @Param(Array(
    "tethys-jackson",
    "pure-jackson",
    "circe-jawn",
    "StringBuilder",
    "json4s-jackson",
    "json4s-native",
    "play-json",
    "spray-json",
    "pushka"
  ))
  var processorName: String = _

  @Benchmark
  def bench: String = {
    DataWriter.instances(processorName).write(data)
  }
}
