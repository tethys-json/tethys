import sbt._

object JsonReaderBuilderGenerator {
  private val packageName = "readers"

  def gen(dir: File): Unit = {
    val targetFile = dir / "tethys" / packageName / "JsonReaderBuilder.scala"

  }

}
