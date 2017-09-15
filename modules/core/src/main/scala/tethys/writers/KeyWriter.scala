package tethys.writers

trait KeyWriter[A] {
  def toKey(value: A): String
}

object KeyWriter {
  implicit lazy val stringKeyWriter: KeyWriter[String] = new KeyWriter[String] {
    override def toKey(value: String): String = value
  }
}
