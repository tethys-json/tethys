package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

import scala.reflect.ClassTag

trait BasicReaders {

  implicit lazy val booleanJsonReader: JsonReader[Boolean] = new JsonReader[Boolean] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Boolean] = {
      processScalar(it)(it.boolean())
    }
  }

  implicit lazy val stringJsonReader: JsonReader[String] = new JsonReader[String] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, String] = {
      processScalar(it)(it.string())
    }
  }

  implicit lazy val charJsonReader: JsonReader[Char] = new JsonReader[Char] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Char] = {
      processScalar(it)(it.string().flatMap {
        case s if s.length == 1 => Some(s.charAt(0))
        case _ => None
      })
    }
  }

  implicit lazy val numberJsonReader: JsonReader[Number] = new JsonReader[Number] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Number] = {
      processScalar(it)(it.number())
    }
  }

  implicit lazy val shortJsonReader: JsonReader[Short] = numberJsonReader.map(_.shortValue())
  implicit lazy val intJsonReader: JsonReader[Int] = numberJsonReader.map(_.intValue())
  implicit lazy val longJsonReader: JsonReader[Long] = numberJsonReader.map(_.longValue())

  implicit lazy val floatJsonReader: JsonReader[Float] = numberJsonReader.map(_.floatValue())
  implicit lazy val doubleJsonReader: JsonReader[Double] = numberJsonReader.map(_.doubleValue())

  implicit lazy val bigDecimalJsonReader: JsonReader[BigDecimal] = numberJsonReader.map {
    case bd: BigDecimal => bd
    case bi: BigInt => BigDecimal(bi)
    case jbd: java.math.BigDecimal => BigDecimal(jbd)
    case jint: java.lang.Integer => BigDecimal(jint)
    case jshort: java.lang.Short => BigDecimal(jshort.longValue())
    case jlong: java.lang.Long => BigDecimal(jlong)
    case jbi: java.math.BigInteger => BigDecimal(jbi)
    case jfloat: java.lang.Float => BigDecimal(jfloat)
    case jdouble: java.lang.Double => BigDecimal(jdouble)
    case num => BigDecimal(num.doubleValue())
  }

  implicit lazy val bigIntJsonReader: JsonReader[BigInt] = numberJsonReader.map {
    case bi: BigInt => bi
    case jbi: java.math.BigInteger => BigInt(jbi)
    case bd: BigDecimal => bd.toBigInt()
    case jbd: java.math.BigDecimal => jbd.toBigInteger
    case jint: java.lang.Integer => BigInt(jint)
    case jshort: java.lang.Short => BigInt(jshort.longValue())
    case jlong: java.lang.Long => BigInt(jlong)
    case num => BigInt(num.longValue())
  }

  implicit lazy val javaShortJsonReader: JsonReader[java.lang.Short] = numberJsonReader.map(_.shortValue())
  implicit lazy val javaIntJsonReader: JsonReader[java.lang.Integer] = numberJsonReader.map(_.intValue())
  implicit lazy val javaLongJsonReader: JsonReader[java.lang.Long] = numberJsonReader.map(_.longValue())
  implicit lazy val javaFloatJsonReader: JsonReader[java.lang.Float] = numberJsonReader.map(_.floatValue())
  implicit lazy val javaDoubleJsonReader: JsonReader[java.lang.Double] = numberJsonReader.map(_.doubleValue())
  implicit lazy val javaBigDecimalJsonReader: JsonReader[java.math.BigDecimal] = bigDecimalJsonReader.map(_.bigDecimal)
  implicit lazy val javaBigIntegerJsonReader: JsonReader[java.math.BigInteger] = bigIntJsonReader.map(_.bigInteger)


  private def processScalar[A](it: TokenIterator)(fun: => Option[A])(implicit fieldName: FieldName, classTag: ClassTag[A]): Either[ReaderError, A] = {
    val either = ReaderError.catchNonFatal {
      val res = fun
      it.nextToken()
      res
    }

    either match {
      case Right(Some(result)) => Right(result)
      case Right(_) => ReaderError.wrongType[A]
      case left => left.asInstanceOf[Either[ReaderError, A]]
    }
  }
}
