package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

trait AllJsonReaders extends OptionReaders {
  implicit lazy val booleanReader: JsonReader[Boolean] =
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

  implicit lazy val stringReader: JsonReader[String] = new JsonReader[String] {
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

  implicit lazy val numberReader: JsonReader[Number] = new JsonReader[Number] {
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

  implicit lazy val shortReader: JsonReader[Short] = new JsonReader[Short] {
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

  implicit lazy val intReader: JsonReader[Int] = new JsonReader[Int] {
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

  implicit lazy val longReader: JsonReader[Long] = new JsonReader[Long] {
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

  implicit lazy val floatReader: JsonReader[Float] = new JsonReader[Float] {
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

  implicit lazy val doubleReader: JsonReader[Double] = new JsonReader[Double] {
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

  implicit lazy val bigDecimalReader: JsonReader[BigDecimal] =
    numberReader.map {
      case bd: BigDecimal            => bd
      case bi: BigInt                => BigDecimal(bi)
      case jbd: java.math.BigDecimal => BigDecimal(jbd)
      case jint: java.lang.Integer   => BigDecimal(jint)
      case jshort: java.lang.Short   => BigDecimal(jshort.longValue())
      case jlong: java.lang.Long     => BigDecimal(jlong)
      case jbi: java.math.BigInteger => BigDecimal(jbi)
      case jfloat: java.lang.Float   => BigDecimal(jfloat.toDouble)
      case jdouble: java.lang.Double => BigDecimal(jdouble)
      case num                       => BigDecimal(num.doubleValue())
    }

  implicit lazy val bigIntReader: JsonReader[BigInt] = numberReader.map {
    case bi: BigInt                => bi
    case jbi: java.math.BigInteger => BigInt(jbi)
    case bd: BigDecimal            => bd.toBigInt
    case jbd: java.math.BigDecimal => jbd.toBigInteger
    case jint: java.lang.Integer   => BigInt(jint)
    case jshort: java.lang.Short   => BigInt(jshort.longValue())
    case jlong: java.lang.Long     => BigInt(jlong)
    case num                       => BigInt(num.longValue())
  }

  implicit lazy val javaBooleanReader: JsonReader[java.lang.Boolean] =
    booleanReader.map(a => a)
  implicit lazy val javaShortReader: JsonReader[java.lang.Short] =
    shortReader.map(a => a)
  implicit lazy val javaIntReader: JsonReader[java.lang.Integer] =
    intReader.map(a => a)
  implicit lazy val javaLongReader: JsonReader[java.lang.Long] =
    longReader.map(a => a)
  implicit lazy val javaFloatReader: JsonReader[java.lang.Float] =
    floatReader.map(a => a)
  implicit lazy val javaDoubleReader: JsonReader[java.lang.Double] =
    doubleReader.map(a => a)
  implicit lazy val javaBigDecimalReader: JsonReader[java.math.BigDecimal] =
    bigDecimalReader.map(_.bigDecimal)
  implicit lazy val javaBigIntegerReader: JsonReader[java.math.BigInteger] =
    bigIntReader.map(_.bigInteger)
  implicit lazy val javaUUIDReader: JsonReader[java.util.UUID] =
    stringReader.map(java.util.UUID.fromString(_))

}
