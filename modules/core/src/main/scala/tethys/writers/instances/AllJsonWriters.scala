package tethys.writers.instances

import tethys.JsonWriter
import tethys.writers.tokens.TokenWriter

trait AllJsonWriters extends OptionWriters with EitherWriters {
  implicit val intWriter: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val longWriter: JsonWriter[Long] = new JsonWriter[Long] {
    override def write(value: Long, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val byteWriter: JsonWriter[Byte] = new JsonWriter[Byte] {
    override def write(value: Byte, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val shortWriter: JsonWriter[Short] = new JsonWriter[Short] {
    override def write(value: Short, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val doubleWriter: JsonWriter[Double] = new JsonWriter[Double] {
    override def write(value: Double, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val floatWriter: JsonWriter[Float] = new JsonWriter[Float] {
    override def write(value: Float, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val bigDecimalWriter: JsonWriter[BigDecimal] =
    new JsonWriter[BigDecimal] {
      override def write(value: BigDecimal, tokenWriter: TokenWriter): Unit =
        tokenWriter.writeNumber(value)
    }

  implicit val bigIntWriter: JsonWriter[BigInt] = new JsonWriter[BigInt] {
    override def write(value: BigInt, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit val booleanWriter: JsonWriter[Boolean] =
    new JsonWriter[Boolean] {
      override def write(value: Boolean, tokenWriter: TokenWriter): Unit =
        tokenWriter.writeBoolean(value)
    }

  implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeString(value)
  }

  implicit val charWriter: JsonWriter[Char] = new JsonWriter[Char] {
    override def write(value: Char, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeString(value.toString)
  }

  implicit val javaIntWriter: JsonWriter[java.lang.Integer] =
    new JsonWriter[java.lang.Integer] {
      override def write(
          value: java.lang.Integer,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaLongWriter: JsonWriter[java.lang.Long] =
    new JsonWriter[java.lang.Long] {
      override def write(
          value: java.lang.Long,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaByteWriter: JsonWriter[java.lang.Byte] =
    new JsonWriter[java.lang.Byte] {
      override def write(
          value: java.lang.Byte,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaShortWriter: JsonWriter[java.lang.Short] =
    new JsonWriter[java.lang.Short] {
      override def write(
          value: java.lang.Short,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaDoubleWriter: JsonWriter[java.lang.Double] =
    new JsonWriter[java.lang.Double] {
      override def write(
          value: java.lang.Double,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaFloatWriter: JsonWriter[java.lang.Float] =
    new JsonWriter[java.lang.Float] {
      override def write(
          value: java.lang.Float,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaBigDecimalWriter: JsonWriter[java.math.BigDecimal] =
    new JsonWriter[java.math.BigDecimal] {
      override def write(
          value: java.math.BigDecimal,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaBigIntegerWriter: JsonWriter[java.math.BigInteger] =
    new JsonWriter[java.math.BigInteger] {
      override def write(
          value: java.math.BigInteger,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit val javaBooleanWriter: JsonWriter[java.lang.Boolean] =
    new JsonWriter[java.lang.Boolean] {
      override def write(
          value: java.lang.Boolean,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeBoolean(value)
    }

  implicit val uuidWriter: JsonWriter[java.util.UUID] =
    new JsonWriter[java.util.UUID] {
      override def write(
          value: java.util.UUID,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeString(value.toString)
    }

  implicit val nullWriter: JsonWriter[Null] = new JsonWriter[Null] {
    override def write(value: Null, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNull()
  }

  implicit val instantWriter: JsonWriter[java.time.Instant] =
    new JsonWriter[java.time.Instant] {
      override def write(
          value: java.time.Instant,
          tokenWriter: TokenWriter
      ): Unit =
        tokenWriter.writeString(value.toString)
    }

  implicit val localDateWriter: JsonWriter[java.time.LocalDate] =
    new JsonWriter[java.time.LocalDate] {
      override def write(
          value: java.time.LocalDate,
          tokenWriter: TokenWriter
      ): Unit =
        tokenWriter.writeString(
          value.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
        )
    }

  implicit lazy val localDateTimeWriter: JsonWriter[java.time.LocalDateTime] =
    new JsonWriter[java.time.LocalDateTime] {
      override def write(
          value: java.time.LocalDateTime,
          tokenWriter: TokenWriter
      ): Unit =
        tokenWriter.writeString(
          value.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        )
    }

  implicit lazy val offsetDateTimeWriter: JsonWriter[java.time.OffsetDateTime] =
    new JsonWriter[java.time.OffsetDateTime] {
      override def write(
          value: java.time.OffsetDateTime,
          tokenWriter: TokenWriter
      ): Unit =
        tokenWriter.writeString(
          value.format(java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        )
    }

  implicit lazy val zonedDateTimeWriter: JsonWriter[java.time.ZonedDateTime] =
    new JsonWriter[java.time.ZonedDateTime] {
      override def write(
          value: java.time.ZonedDateTime,
          tokenWriter: TokenWriter
      ): Unit =
        tokenWriter.writeString(
          value.format(java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME)
        )
    }
}
