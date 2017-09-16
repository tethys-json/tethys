package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.tokens.{Token, TokenIterator}
import tethys.readers.{FieldName, ReaderError}

import scala.reflect.ClassTag

trait BasicReaders {

  implicit lazy val booleanJsonReader: JsonReader[Boolean] = new ScalarReader[Boolean] {
    override protected def isCorrectToken(token: Token) = token.isBooleanValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.boolean()
  }

  implicit lazy val stringJsonReader: JsonReader[String] = new ScalarReader[String] {
    override protected def isCorrectToken(token: Token) = token.isStringValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.string()
  }

  implicit lazy val charJsonReader: JsonReader[Char] = new ScalarReader[Char] {
    override protected def isCorrectToken(token: Token) = token.isStringValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = {
      val s = it.string()
      if(s.length == 1) s.charAt(0)
      else ReaderError.wrongType[Char]
    }
  }

  implicit lazy val numberJsonReader: JsonReader[Number] = new ScalarReader[Number] {
    override protected def isCorrectToken(token: Token) = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.number()
  }

  implicit lazy val shortJsonReader: JsonReader[Short] = new ScalarReader[Short] {
    override protected def isCorrectToken(token: Token) = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.short()
  }
  implicit lazy val intJsonReader: JsonReader[Int] = new ScalarReader[Int] {
    override protected def isCorrectToken(token: Token) = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.int()
  }
  implicit lazy val longJsonReader: JsonReader[Long] = new ScalarReader[Long] {
    override protected def isCorrectToken(token: Token) = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.long()
  }

  implicit lazy val floatJsonReader: JsonReader[Float] = new ScalarReader[Float] {
    override protected def isCorrectToken(token: Token) = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.float()
  }
  implicit lazy val doubleJsonReader: JsonReader[Double] = new ScalarReader[Double] {
    override protected def isCorrectToken(token: Token) = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName) = it.double()
  }

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

  implicit lazy val javaShortJsonReader: JsonReader[java.lang.Short] = shortJsonReader.map(a => a)
  implicit lazy val javaIntJsonReader: JsonReader[java.lang.Integer] = intJsonReader.map(a => a)
  implicit lazy val javaLongJsonReader: JsonReader[java.lang.Long] = longJsonReader.map(a => a)
  implicit lazy val javaFloatJsonReader: JsonReader[java.lang.Float] = floatJsonReader.map(a => a)
  implicit lazy val javaDoubleJsonReader: JsonReader[java.lang.Double] = doubleJsonReader.map(a => a)
  implicit lazy val javaBigDecimalJsonReader: JsonReader[java.math.BigDecimal] = bigDecimalJsonReader.map(_.bigDecimal)
  implicit lazy val javaBigIntegerJsonReader: JsonReader[java.math.BigInteger] = bigIntJsonReader.map(_.bigInteger)


  private abstract class ScalarReader[A](implicit ct: ClassTag[A]) extends JsonReader[A] {

    protected def isCorrectToken(token: Token): Boolean

    protected def value(it: TokenIterator)(implicit fieldName: FieldName): A

    override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
      if(isCorrectToken(it.currentToken())) {
        val res = value(it)
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[A]
      }
    }
  }
}
