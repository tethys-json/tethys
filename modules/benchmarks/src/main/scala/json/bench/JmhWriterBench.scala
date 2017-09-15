package json.bench

import java.util.concurrent.TimeUnit

import json.bench.model.Data
import org.openjdk.jmh.annotations._

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 20, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 60, timeUnit = TimeUnit.SECONDS)
@Fork(4)
@State(Scope.Thread)
class JmhWriterBench {
  @Param(Array(
    "10",
    "100",
    "1000",
    "5000",
    "10000",
    "50000",
    "100000"
  ))
  var size: Int = _

  val seed = 10000
  var data: Seq[Data] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    data = Data.dataSamples(size, seed)
  }

  @Param(Array(
    "tethys-jackson",
    "circe-jawn",
    "StringBuilder",
    "pure-jackson",
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
