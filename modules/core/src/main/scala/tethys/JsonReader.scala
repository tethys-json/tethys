package tethys

import tethys.commons.{LowPriorityInstance, Token}
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, JsonReaderBuilder, KeyReader, ReaderError}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

trait JsonReader[A] {
  self =>

  def read(it: TokenIterator)(implicit fieldName: FieldName): A

  def defaultValue: Option[A] = None

  def map[B](fun: A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B = fun(self.read(it))
  }
}

object JsonReader extends MidPriorityReaders {
  def apply[A](implicit jsonReader: JsonReader[A]): JsonReader[A] = jsonReader

  val builder: JsonReaderBuilder.type = JsonReaderBuilder

  implicit lazy val booleanReader: JsonReader[Boolean] = new ScalarReader[Boolean] {
    override protected def isCorrectToken(token: Token): Boolean = token.isBooleanValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Boolean = it.boolean()
  }

  implicit lazy val stringReader: JsonReader[String] = new ScalarReader[String] {
    override protected def isCorrectToken(token: Token): Boolean = token.isStringValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): String = it.string()
  }

  implicit lazy val charReader: JsonReader[Char] = new ScalarReader[Char] {
    override protected def isCorrectToken(token: Token): Boolean = token.isStringValue

    override protected def value(it: TokenIterator)(implicit fieldName: FieldName): Char = {
      val s = it.string()
      if(s.length == 1) s.charAt(0)
      else ReaderError.wrongType[Char]
    }
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

  implicit def mapReader[K, A](implicit keyReader: KeyReader[K], jsonReader: JsonReader[A]): JsonReader[Map[K, A]] = new JsonReader[Map[K, A]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Map[K, A] = {
      if(it.currentToken().isObjectStart) recRead(it.next(), Map.newBuilder[K, A])(fieldName)
      else ReaderError.wrongType[Map[K, A]]
    }

    @tailrec
    private def recRead(it: TokenIterator, builder: mutable.Builder[(K, A), Map[K, A]])
                       (fieldName: FieldName): Map[K, A] = {
      it.currentToken() match {
        case token if token.isObjectEnd =>
          it.nextToken()
          builder.result()
        case token if token.isFieldName =>
          val name = it.fieldName()
          implicit val nextFieldName: FieldName = fieldName.appendFieldName(name)

          recRead(it, builder += keyReader.read(name) -> jsonReader.read(it.next()))(fieldName)

        case _ => ReaderError.wrongJson(fieldName)
      }
    }
  }

  implicit def optionReader[A](implicit jsonReader: JsonReader[A]): JsonReader[Option[A]] = new JsonReader[Option[A]] {

    override val defaultValue: Option[Option[A]] = Some(None)

    override def read(it: TokenIterator)(implicit fieldName: FieldName): Option[A] = {
      if(it.currentToken().isNullValue) {
        it.nextToken()
        None
      } else {
        Some(jsonReader.read(it))
      }
    }
  }
}

private[tethys] trait MidPriorityReaders extends LowPriorityJsonReaders {
  implicit def genTraversableReader[A, C[X] <: Traversable[X]](implicit
                                                               jsonReader: JsonReader[A],
                                                               cbf: CanBuildFrom[Nothing, A, C[A]],
                                                               classTag: ClassTag[C[A]]
                                                              ): JsonReader[C[A]] = new JsonReader[C[A]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): C[A] = {
      if(it.currentToken().isArrayStart) recRead(0, it.next(), cbf())
      else ReaderError.wrongType[C[A]]
    }

    @tailrec
    private def recRead(i: Int, it: TokenIterator, builder: mutable.Builder[A, C[A]])
                       (implicit fieldName: FieldName): C[A] = {
      it.currentToken() match {
        case token if token.isEmpty => ReaderError.wrongJson
        case token if token.isArrayEnd =>
          it.nextToken()
          builder.result()
        case _ =>
          recRead(i + 1, it, builder += jsonReader.read(it)(fieldName.appendArrayIndex(i)))
      }
    }
  }
}

private[tethys] trait LowPriorityJsonReaders {
  implicit final def lowPriorityReader[A](implicit lowPriorityInstance: LowPriorityInstance[JsonReader[A]]): JsonReader[A] = {
    lowPriorityInstance.instance
  }
}
