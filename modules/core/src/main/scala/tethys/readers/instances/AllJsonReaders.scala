package tethys.readers.instances

import tethys.commons.Token
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}
import tethys.{JsonReader, specializations}

import scala.reflect.ClassTag

trait AllJsonReaders extends OptionReaders {
  implicit lazy val booleanReader: JsonReader[Boolean] = new ScalarReader[Boolean] {
    override protected def isCorrectToken(token: Token): Boolean = token.isBooleanValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Boolean = it.boolean()
  }

  implicit lazy val stringReader: JsonReader[String] = new ScalarReader[String] {
    override protected def isCorrectToken(token: Token): Boolean = token.isStringValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): String = it.string()
  }

  implicit lazy val numberReader: JsonReader[Number] = new ScalarReader[Number] {
    override protected def isCorrectToken(token: Token): Boolean = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Number = it.number()
  }

  implicit lazy val shortReader: JsonReader[Short] = new ScalarReader[Short] {
    override protected def isCorrectToken(token: Token): Boolean = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Short = it.short()
  }

  implicit lazy val intReader: JsonReader[Int] = new ScalarReader[Int] {
    override protected def isCorrectToken(token: Token): Boolean = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Int = it.int()
  }

  implicit lazy val longReader: JsonReader[Long] = new ScalarReader[Long] {
    override protected def isCorrectToken(token: Token): Boolean = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Long = it.long()
  }

  implicit lazy val floatReader: JsonReader[Float] = new ScalarReader[Float] {
    override protected def isCorrectToken(token: Token): Boolean = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Float = it.float()
  }

  implicit lazy val doubleReader: JsonReader[Double] = new ScalarReader[Double] {
    override protected def isCorrectToken(token: Token): Boolean = token.isNumberValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Double = it.double()
  }

  implicit lazy val bigDecimalReader: JsonReader[BigDecimal] = numberReader.map {
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

  implicit lazy val bigIntReader: JsonReader[BigInt] = numberReader.map {
    case bi: BigInt => bi
    case jbi: java.math.BigInteger => BigInt(jbi)
    case bd: BigDecimal => bd.toBigInt()
    case jbd: java.math.BigDecimal => jbd.toBigInteger
    case jint: java.lang.Integer => BigInt(jint)
    case jshort: java.lang.Short => BigInt(jshort.longValue())
    case jlong: java.lang.Long => BigInt(jlong)
    case num => BigInt(num.longValue())
  }

  implicit lazy val javaShortReader: JsonReader[java.lang.Short] = shortReader.map(a => a)
  implicit lazy val javaIntReader: JsonReader[java.lang.Integer] = intReader.map(a => a)
  implicit lazy val javaLongReader: JsonReader[java.lang.Long] = longReader.map(a => a)
  implicit lazy val javaFloatReader: JsonReader[java.lang.Float] = floatReader.map(a => a)
  implicit lazy val javaDoubleReader: JsonReader[java.lang.Double] = doubleReader.map(a => a)
  implicit lazy val javaBigDecimalReader: JsonReader[java.math.BigDecimal] = bigDecimalReader.map(_.bigDecimal)
  implicit lazy val javaBigIntegerReader: JsonReader[java.math.BigInteger] = bigIntReader.map(_.bigInteger)


  private abstract class ScalarReader[@specialized(specializations) A](implicit ct: ClassTag[A]) extends JsonReader[A] {

    protected def isCorrectToken(token: Token): Boolean

    protected def value(it: TokenIterator)(implicit fieldName: FieldName): A

    override def read(it: TokenIterator)(implicit fieldName: FieldName): A = {
      if (isCorrectToken(it.currentToken())) {
        val res = value(it)
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[A]
      }
    }
  }

}
