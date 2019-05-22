package json.bench

import java.util.concurrent.TimeUnit

import json.bench.circe.CirceBench
import json.bench.model.Data
import org.openjdk.jmh.annotations.{State, _}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 2, time = 30, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 4, time = 30, timeUnit = TimeUnit.SECONDS)
@Fork(2)
@State(Scope.Benchmark)
class JmhReaderBench {
  @Param(Array(
    "10",
    "100",
    "1000",
    "10000",
    "100000"
  ))
  var arraySize: Int = _

  val seed = 10000

  var data: String =_

  @Setup(Level.Trial)
  def setup(): Unit = {
    data = CirceBench.CirceDataWriter.write(Data.dataSamples(arraySize, seed))
  }

  @Param(Array(
    "tethys-jackson",
    "pure-jackson",
    "circe-jawn",
    "circe-jackson",
    "json4s-jackson",
    "json4s-native",
    "play-json",
    "spray-json",
    "pushka"
  ))
  var processorName: String = _

  @Benchmark
  def bench: Seq[Data] = {
    DataReader.instances(processorName).read(data)
  }
}
