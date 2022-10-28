package tethys.writers

trait KeyWriter[A] {
  def toKey(value: A): String
}

object KeyWriter {
  implicit lazy val stringKeyWriter: KeyWriter[String] = identity

  implicit lazy val uuidKeyWriter: KeyWriter[java.util.UUID] = _.toString

  implicit lazy val intKeyWriter: KeyWriter[Int] = _.toString

  implicit lazy val longKeyWriter: KeyWriter[Long] = _.toString
}
