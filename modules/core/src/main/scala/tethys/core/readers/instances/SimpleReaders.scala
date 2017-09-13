package tethys.core.readers.instances

import tethys.core.readers.tokens.TokenIterator
import tethys.core.readers.{FieldName, JsonReader, ReaderError}

trait SimpleReaders {

  implicit lazy val booleanJsonReader: JsonReader[Boolean] = new JsonReader[Boolean] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Boolean] = {
      ReaderError.processScalar(it.boolean())
    }
  }

  implicit lazy val stringJsonReader: JsonReader[String] = new JsonReader[String] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, String] = {
      ReaderError.processScalar(it.string())
    }
  }

  implicit lazy val numberJsonReader: JsonReader[Number] = new JsonReader[Number] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Number] = {
      ReaderError.processScalar(it.number())
    }
  }

  implicit lazy val shortJsonReader: JsonReader[Short] = numberJsonReader.map(_.shortValue())
  implicit lazy val intJsonReader: JsonReader[Int] = numberJsonReader.map(_.intValue())
  implicit lazy val longJsonReader: JsonReader[Long] = numberJsonReader.map(_.longValue())

  implicit lazy val floatJsonReader: JsonReader[Float] = numberJsonReader.map(_.floatValue())
  implicit lazy val doubleJsonReader: JsonReader[Double] = numberJsonReader.map(_.doubleValue())

  implicit lazy val bigDecimalJsonReader: JsonReader[BigDecimal] = numberJsonReader.map {
    case bd: BigDecimal => bd
    case jbd: java.math.BigDecimal => BigDecimal(jbd)
    case jint: java.lang.Integer => BigDecimal(jint)
    case jshort: java.lang.Short => BigDecimal(jshort.longValue())
    case jlong: java.lang.Long => BigDecimal(jlong)
    case jfloat: java.lang.Float => BigDecimal(jfloat)
    case jdouble: java.lang.Double => BigDecimal(jdouble)
    case num => BigDecimal(num.doubleValue())
  }

}
