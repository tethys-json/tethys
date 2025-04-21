package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

trait AllJsonReaders extends OptionReaders {
  implicit val booleanReader: JsonReader[Boolean] =
    new JsonReader[Boolean] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): Boolean = {
        if (it.currentToken().isBooleanValue) {
          val res = it.boolean()
          it.next()
          res
        } else {
          ReaderError.wrongJson(
            s"Expected boolean value but found: ${it.currentToken()}"
          )
        }
      }
    }

  implicit val stringReader: JsonReader[String] = new JsonReader[String] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): String = {
      if (it.currentToken().isStringValue) {
        val res = it.string()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected string value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val charReader: JsonReader[Char] = stringReader.mapWithField {
    implicit fieldName =>
      {
        case s if s.length == 1 => s.head
        case s => ReaderError.wrongJson(s"Expected char value but found: $s")
      }
  }

  implicit val numberReader: JsonReader[Number] = new JsonReader[Number] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Number = {
      if (it.currentToken().isNumberValue) {
        val res = it.number()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected number value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val byteReader: JsonReader[Byte] = new JsonReader[Byte] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Byte = {
      if (it.currentToken().isNumberValue) {
        val res = it.byte()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected byte value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val shortReader: JsonReader[Short] = new JsonReader[Short] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Short = {
      if (it.currentToken().isNumberValue) {
        val res = it.short()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected short value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val intReader: JsonReader[Int] = new JsonReader[Int] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Int = {
      if (it.currentToken().isNumberValue) {
        val res = it.int()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected int value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val longReader: JsonReader[Long] = new JsonReader[Long] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Long = {
      if (it.currentToken().isNumberValue) {
        val res = it.long()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected long value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val floatReader: JsonReader[Float] = new JsonReader[Float] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Float = {
      if (it.currentToken().isNumberValue) {
        val res = it.float()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected float value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val doubleReader: JsonReader[Double] = new JsonReader[Double] {
    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Double = {
      if (it.currentToken().isNumberValue) {
        val res = it.double()
        it.next()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected double value but found: ${it.currentToken()}"
        )
      }
    }
  }

  implicit val bigDecimalReader: JsonReader[BigDecimal] =
    numberReader.map {
      case bd: BigDecimal            => bd
      case bi: BigInt                => BigDecimal(bi)
      case jbd: java.math.BigDecimal => BigDecimal(jbd)
      case jint: java.lang.Integer   => BigDecimal(jint)
      case jbyte: java.lang.Byte     => BigDecimal(jbyte.longValue())
      case jshort: java.lang.Short   => BigDecimal(jshort.longValue())
      case jlong: java.lang.Long     => BigDecimal(jlong)
      case jbi: java.math.BigInteger => BigDecimal(jbi)
      case jfloat: java.lang.Float   => BigDecimal(jfloat.toDouble)
      case jdouble: java.lang.Double => BigDecimal(jdouble)
      case num                       => BigDecimal(num.doubleValue())
    }

  implicit val bigIntReader: JsonReader[BigInt] = numberReader.map {
    case bi: BigInt                => bi
    case jbi: java.math.BigInteger => BigInt(jbi)
    case bd: BigDecimal            => bd.toBigInt
    case jbd: java.math.BigDecimal => jbd.toBigInteger
    case jint: java.lang.Integer   => BigInt(jint)
    case jbyte: java.lang.Byte     => BigInt(jbyte.longValue())
    case jshort: java.lang.Short   => BigInt(jshort.longValue())
    case jlong: java.lang.Long     => BigInt(jlong)
    case num                       => BigInt(num.longValue())
  }

  implicit val javaBooleanReader: JsonReader[java.lang.Boolean] =
    booleanReader.map(a => a)
  implicit val javaByteReader: JsonReader[java.lang.Byte] =
    byteReader.map(a => a)
  implicit val javaShortReader: JsonReader[java.lang.Short] =
    shortReader.map(a => a)
  implicit val javaIntReader: JsonReader[java.lang.Integer] =
    intReader.map(a => a)
  implicit val javaLongReader: JsonReader[java.lang.Long] =
    longReader.map(a => a)
  implicit val javaFloatReader: JsonReader[java.lang.Float] =
    floatReader.map(a => a)
  implicit val javaDoubleReader: JsonReader[java.lang.Double] =
    doubleReader.map(a => a)
  implicit val javaBigDecimalReader: JsonReader[java.math.BigDecimal] =
    bigDecimalReader.map(_.bigDecimal)
  implicit val javaBigIntegerReader: JsonReader[java.math.BigInteger] =
    bigIntReader.map(_.bigInteger)
  implicit val javaUUIDReader: JsonReader[java.util.UUID] =
    stringReader.map(java.util.UUID.fromString(_))

  implicit val javaInstantReader: JsonReader[java.time.Instant] =
    stringReader.map(java.time.Instant.parse)
  implicit val javaLocalDateReader: JsonReader[java.time.LocalDate] =
    stringReader.map(
      java.time.LocalDate
        .parse(_, java.time.format.DateTimeFormatter.ISO_LOCAL_DATE)
    )
  implicit val javaLocalDateTimeReader
      : JsonReader[java.time.LocalDateTime] =
    stringReader.map(
      java.time.LocalDateTime
        .parse(_, java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
    )
  implicit val javaOffsetDateTimeReader
      : JsonReader[java.time.OffsetDateTime] =
    stringReader.map(
      java.time.OffsetDateTime
        .parse(_, java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME)
    )
  implicit val javaZonedDateTimeReader
      : JsonReader[java.time.ZonedDateTime] =
    stringReader.map(
      java.time.ZonedDateTime
        .parse(_, java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME)
    )

}
