package tethys.writers

@FunctionalInterface
trait KeyWriter[A] {
  def toKey(value: A): String
}

object KeyWriter {
  implicit val stringKeyWriter: KeyWriter[String] = identity

  implicit val uuidKeyWriter: KeyWriter[java.util.UUID] = _.toString

  implicit val intKeyWriter: KeyWriter[Int] = _.toString

  implicit val longKeyWriter: KeyWriter[Long] = _.toString

  implicit val instantKeyWriter: KeyWriter[java.time.Instant] = _.toString

  implicit val localDateKeyWriter: KeyWriter[java.time.LocalDate] =
    _.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)

  implicit val localDateTimeKeyWriter: KeyWriter[java.time.LocalDateTime] =
    _.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  implicit val offsetDateTimeKeyWriter: KeyWriter[java.time.OffsetDateTime] =
    _.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  implicit val zonedDateTimeKeyWriter: KeyWriter[java.time.ZonedDateTime] =
    _.format(java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME)
}
