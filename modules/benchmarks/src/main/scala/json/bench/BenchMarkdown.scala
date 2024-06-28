package json.bench

import java.nio.file.{Files, Paths}

import org.knowm.xchart.BitmapEncoder.BitmapFormat
import org.knowm.xchart.{BitmapEncoder, CategoryChart, CategoryChartBuilder}
import tethys._
import tethys.derivation.semiauto._
import tethys.jackson._
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator

import scala.collection.mutable

object BenchMarkdown {
  val nameColumn = "processorName"
  val sizeColumn = "jsonSize"

  val benchmarksOrdering: Ordering[String] = Ordering.by[String, Int] {
    case "Parsing" | "json.bench.JmhReaderBench.bench" => 1
    case "Writing" | "json.bench.JmhWriterBench.bench" => 2
    case _                                             => 3
  }

  val processorsOrdering: Ordering[String] = Ordering.by[String, Int] {
    case "tethys-jackson"          => 1
    case "pure-jackson"            => 2
    case "circe"                   => 3
    case "circe-jawn"              => 4
    case "circe-jackson"           => 5
    case "java.lang.StringBuilder" => 6
    case "scala.StringBuilder"     => 7
    case "json4s-jackson"          => 8
    case "json4s-native"           => 9
    case "play-json"               => 10
    case "spray-json"              => 11
    case "pushka"                  => 12
    case _                         => 13
  }

  val namesMapping = Map(
    "json.bench.JmhReaderBench.bench" -> "Parsing",
    "json.bench.JmhWriterBench.bench" -> "Writing"
  )

  implicit val mbScore: JsonReader[Either[String, Double]] =
    new JsonReader[Either[String, Double]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): Either[String, Double] = {
        if (it.currentToken().isNumberValue)
          Right(Math.round(JsonReader.doubleReader.read(it) * 1000) / 1000.0)
        else Left(JsonReader.stringReader.read(it))
      }
    }

  implicit val primaryMetricsReader: JsonReader[PrimaryMetrics] =
    jsonReader[PrimaryMetrics]
  implicit val benchmarkReader: JsonReader[Benchmark] = jsonReader[Benchmark]

  case class Benchmark(
      benchmark: String,
      mode: String,
      params: mutable.LinkedHashMap[String, String],
      primaryMetric: PrimaryMetrics
  )
  case class PrimaryMetrics(
      score: Either[String, Double],
      scoreError: Either[String, Double]
  )

  def readBenchmarks(dir: String, file: String): Seq[Benchmark] = {
    val jhmResultsPath = Paths.get(dir, file)
    val json = new String(Files.readAllBytes(jhmResultsPath), "UTF-8")
    json.jsonAs[Seq[Benchmark]].fold(throw _, identity)
  }

  def main(args: Array[String]): Unit = {
    val List(dir) = args.toList
    val benchs = readBenchmarks(dir, "jmh-reader.json") ++ readBenchmarks(
      dir,
      "jmh-writer.json"
    )
    val grouped = benchs.groupBy(_.benchmark)
    val mainTables = grouped.toList.sortBy(_._1)(benchmarksOrdering).map {
      case (name, benchmarks) =>
        val rows = benchmarks
          .map(_.params(nameColumn))
          .distinct
          .sorted(processorsOrdering)
        val colls = benchmarks.map(_.params(sizeColumn)).distinct
        val data = benchmarks.map { b =>
          (b.params(nameColumn), b.params(sizeColumn)) -> b.primaryMetric.score
            .fold(identity, _.toString)
        }.toMap

        s"""
           |# ${namesMapping.getOrElse(name, name)}
           |
           |${table(rows, colls, data)}
           |${chart(name, benchmarks, dir)}
         """.stripMargin
    }

    val readmeContent = mainTables.mkString("\n\n")
    Files.write(Paths.get(dir, "README.md"), readmeContent.getBytes("UTF-8"))
  }

  private def chart(name: String, bs: Seq[Benchmark], dir: String): String = {
    val title = namesMapping.getOrElse(name, name)
    val images = Paths.get(dir, "images")
    if (Files.notExists(images)) {
      Files.createDirectory(images)
    }
    val imgPath = dir + "/images/" + title + "Performance"
    val chart = buildPerformanceChart(title, bs)
    BitmapEncoder.saveBitmap(chart, imgPath, BitmapFormat.PNG)
    s"""
       |![${title}Performance](./images/${title}Performance.png)
     """.stripMargin
  }

  private def table(
      rows: Seq[String],
      columns: Seq[String],
      data: Map[(String, String), String]
  ): String = {
    val header = columns.foldLeft("name \\ size")(_ + "|" + _)
    val line = columns.map(_ => "---").foldLeft("---")(_ + "|" + _)
    val dataLines = rows.map { row =>
      columns
        .map(col => data.getOrElse((row, col), " "))
        .foldLeft(row)(_ + "|" + _)
    }
    dataLines.foldLeft(header + "\n" + line)(_ + "\n" + _)
  }

  private def buildPerformanceChart(
      title: String,
      benchmarks: Seq[Benchmark]
  ): CategoryChart = {
    val chart: CategoryChart = new CategoryChartBuilder()
      .width(940)
      .height(400)
      .title(title)
      .yAxisTitle("ops/s normalized")
      .xAxisTitle("size")
      .build()

    val maxs = benchmarks.groupBy(_.params(sizeColumn)).map { case (size, bs) =>
      size -> bs.map(_.primaryMetric.score.fold(_ => 0.0, identity)).max
    }

    benchmarks
      .groupBy(_.params(nameColumn))
      .toList
      .sortBy(_._1)(processorsOrdering)
      .foreach { case (name, bs) =>
        import scala.collection.JavaConverters._

        val data = bs.map { b =>
          val size = b.params(sizeColumn)
          size -> (b.primaryMetric.score.fold(_ => 0.0, identity) / maxs(size))
        }
        val xData = data.map(_._1).asJava
        val yData = data.map(t => Double.box(t._2)).asJava
        chart.addSeries(name, xData, yData)
      }

    chart
  }
}
