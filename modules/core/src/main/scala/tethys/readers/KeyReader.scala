package tethys.readers

@specialized(tethys.specializations)
trait KeyReader[A] {
  def read(s: String)(implicit fieldName: FieldName): A
}

object KeyReader {
  implicit lazy val stringKeyReader: KeyReader[String] = new KeyReader[String] {
    override def read(s: String)(implicit fieldName: FieldName): String = s
  }

  implicit lazy val uuidKeyReader: KeyReader[java.util.UUID] =
    new KeyReader[java.util.UUID] {
      override def read(s: String)(implicit
          fieldName: FieldName
      ): java.util.UUID = java.util.UUID.fromString(s)
    }

  implicit lazy val intKeyReader: KeyReader[Int] = new KeyReader[Int] {
    override def read(s: String)(implicit fieldName: FieldName): Int = s.toInt
  }

  implicit lazy val longKeyReader: KeyReader[Long] = new KeyReader[Long] {
    override def read(s: String)(implicit fieldName: FieldName): Long = s.toLong
  }

  implicit lazy val instantKeyReader: KeyReader[java.time.Instant] =
    new KeyReader[java.time.Instant] {
      override def read(s: String)(implicit
          fieldName: FieldName
      ): java.time.Instant = java.time.Instant.parse(s)
    }

  implicit lazy val localDateKeyReader: KeyReader[java.time.LocalDate] =
    new KeyReader[java.time.LocalDate] {
      override def read(
          s: String
      )(implicit fieldName: FieldName): java.time.LocalDate =
        java.time.LocalDate
          .parse(s, java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
    }

  implicit lazy val localDateTimeKeyReader: KeyReader[java.time.LocalDateTime] =
    new KeyReader[java.time.LocalDateTime] {
      override def read(
          s: String
      )(implicit fieldName: FieldName): java.time.LocalDateTime =
        java.time.LocalDateTime
          .parse(s, java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    }

  implicit lazy val offsetDateTimeKeyReader
      : KeyReader[java.time.OffsetDateTime] =
    new KeyReader[java.time.OffsetDateTime] {
      override def read(
          s: String
      )(implicit fieldName: FieldName): java.time.OffsetDateTime =
        java.time.OffsetDateTime
          .parse(s, java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    }

  implicit lazy val zonedDateTimeKeyReader: KeyReader[java.time.ZonedDateTime] =
    new KeyReader[java.time.ZonedDateTime] {
      override def read(
          s: String
      )(implicit fieldName: FieldName): java.time.ZonedDateTime =
        java.time.ZonedDateTime
          .parse(s, java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME)
    }
}
