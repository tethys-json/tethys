package tethys.writers

@FunctionalInterface
trait KeyWriter[A]:
  def toKey(value: A): String

object KeyWriter:
  implicit lazy val stringKeyWriter: KeyWriter[String] = identity

  implicit lazy val uuidKeyWriter: KeyWriter[java.util.UUID] = _.toString

  implicit lazy val intKeyWriter: KeyWriter[Int] = _.toString

  implicit lazy val longKeyWriter: KeyWriter[Long] = _.toString

  implicit lazy val instantKeyWriter: KeyWriter[java.time.Instant] = _.toString

  implicit lazy val localDateKeyWriter: KeyWriter[java.time.LocalDate] =
    _.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)

  implicit lazy val localDateTimeKeyWriter: KeyWriter[java.time.LocalDateTime] =
    _.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  implicit lazy val offsetDateTimeKeyWriter
      : KeyWriter[java.time.OffsetDateTime] =
    _.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  implicit lazy val zonedDateTimeKeyWriter: KeyWriter[java.time.ZonedDateTime] =
    _.format(java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME)
