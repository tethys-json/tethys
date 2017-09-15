package json.bench

import java.util.concurrent.TimeUnit

import json.bench.circe.CirceBench
import json.bench.model.Data
import org.openjdk.jmh.annotations.{State, _}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 2, time = 20, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 30, timeUnit = TimeUnit.SECONDS)
@Fork(4)
@State(Scope.Benchmark)
class JmhReaderBench {

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

  var data: String =_

  @Setup(Level.Trial)
  def setup(): Unit = {
    data = CirceBench.CirceDataProcessor.write(Data.dataSamples(size, seed))
  }

  @Param(Array(
    "tethys-jackson",
    "circe-jawn",
    "pure-jackson",
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
