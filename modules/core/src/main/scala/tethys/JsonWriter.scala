package tethys

import tethys.commons.LowPriorityInstance
import tethys.writers.KeyWriter
import tethys.writers.instances.SimpleJsonObjectWriter
import tethys.writers.tokens.TokenWriter

import scala.collection.GenIterableLike
import scala.language.higherKinds

trait JsonWriter[A] {
  self =>

  def write(name: String, value: A, tokenWriter: TokenWriter): Unit = {
    tokenWriter.writeFieldName(name)
    write(value, tokenWriter)
  }

  def write(value: A, tokenWriter: TokenWriter): Unit

  def contramap[B](fun: B => A): JsonWriter[B] = new JsonWriter[B] {
    override def write(name: String, value: B, tokenWriter: TokenWriter): Unit = {
      self.write(name, fun(value), tokenWriter)
    }

    override def write(value: B, tokenWriter: TokenWriter): Unit = {
      self.write(fun(value), tokenWriter)
    }
  }
}

object JsonWriter extends MidPriorityJsonWriters {
  def apply[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[A] = jsonWriter

  def obj[A]: SimpleJsonObjectWriter[A] = SimpleJsonObjectWriter[A]

  implicit lazy val intWriter: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val longWriter: JsonWriter[Long] = new JsonWriter[Long] {
    override def write(value: Long, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val shortWriter: JsonWriter[Short] = new JsonWriter[Short] {
    override def write(value: Short, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val doubleWriter: JsonWriter[Double] = new JsonWriter[Double] {
    override def write(value: Double, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val floatWriter: JsonWriter[Float] = new JsonWriter[Float] {
    override def write(value: Float, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val bigDecimalWriter: JsonWriter[BigDecimal] = new JsonWriter[BigDecimal] {
    override def write(value: BigDecimal, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val bigIntWriter: JsonWriter[BigInt] = new JsonWriter[BigInt] {
    override def write(value: BigInt, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val booleanWriter: JsonWriter[Boolean] = new JsonWriter[Boolean] {
    override def write(value: Boolean, tokenWriter: TokenWriter): Unit = tokenWriter.writeBoolean(value)
  }

  implicit lazy val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String, tokenWriter: TokenWriter): Unit = tokenWriter.writeString(value)
  }

  implicit lazy val charWriter: JsonWriter[Char] = new JsonWriter[Char] {
    override def write(value: Char, tokenWriter: TokenWriter): Unit = tokenWriter.writeString(value.toString)
  }

  implicit lazy val javaIntWriter: JsonWriter[java.lang.Integer] = new JsonWriter[java.lang.Integer] {
    override def write(value: java.lang.Integer, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaLongWriter: JsonWriter[java.lang.Long] = new JsonWriter[java.lang.Long] {
    override def write(value: java.lang.Long, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaShortWriter: JsonWriter[java.lang.Short] = new JsonWriter[java.lang.Short] {
    override def write(value: java.lang.Short, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaDoubleWriter: JsonWriter[java.lang.Double] = new JsonWriter[java.lang.Double] {
    override def write(value: java.lang.Double, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaFloatWriter: JsonWriter[java.lang.Float] = new JsonWriter[java.lang.Float] {
    override def write(value: java.lang.Float, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaBigDecimalWriter: JsonWriter[java.math.BigDecimal] = new JsonWriter[java.math.BigDecimal] {
    override def write(value: java.math.BigDecimal, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaBigIntegerWriter: JsonWriter[java.math.BigInteger] = new JsonWriter[java.math.BigInteger] {
    override def write(value: java.math.BigInteger, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaBooleanWriter: JsonWriter[java.lang.Boolean] = new JsonWriter[java.lang.Boolean] {
    override def write(value: java.lang.Boolean, tokenWriter: TokenWriter): Unit = tokenWriter.writeBoolean(value)
  }

  implicit def mapWriter[K, A](implicit keyWriter: KeyWriter[K], valueWriter: JsonWriter[A]): JsonObjectWriter[Map[K, A]] = new JsonObjectWriter[Map[K, A]] {
    override def writeValues(value: Map[K, A], tokenWriter: TokenWriter): Unit = {
      val valueIterator = value.iterator
      while(valueIterator.hasNext) {
        val v = valueIterator.next()
        tokenWriter.writeFieldName(keyWriter.toKey(v._1))
        valueWriter.write(v._2, tokenWriter)
      }
    }
  }

  implicit lazy val noneWriter: JsonWriter[None.type] = new JsonWriter[None.type] {
    override def write(name: String, value: None.type, tokenWriter: TokenWriter): Unit = ()
    override def write(value: None.type, tokenWriter: TokenWriter): Unit = tokenWriter.writeNull()
  }

  implicit def someWriter[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[Some[A]] = new JsonWriter[Some[A]] {
    override def write(value: Some[A], tokenWriter: TokenWriter): Unit = {
      jsonWriter.write(value.get, tokenWriter)
    }
  }

  implicit def optionalWriter[A](implicit valueWriter: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {

    override def write(name: String, value: Option[A], tokenWriter: TokenWriter): Unit = {
      if(value.nonEmpty) {
        valueWriter.write(name, value.get, tokenWriter)
      }
    }

    override def write(value: Option[A], tokenWriter: TokenWriter): Unit = {
      if(value.isEmpty) tokenWriter.writeNull()
      else valueWriter.write(value.get, tokenWriter)
    }
  }
}

private[tethys] trait MidPriorityJsonWriters extends LowPriorityJsonWriters {
  final implicit def iteratableWriter[A, C[X] <: GenIterableLike[X, C[X]]](implicit valueWriter: JsonWriter[A]): JsonWriter[C[A]] = new IteratableWriter[A, C](valueWriter) {
    override def iterator(c: C[A]): Iterator[A] = c.iterator
  }

  abstract class IteratableWriter[A, C[_]](valueWriter: JsonWriter[A]) extends JsonWriter[C[A]] {
    def iterator(c: C[A]): Iterator[A]

    override def write(value: C[A], tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeArrayStart()

      val valueIterator = iterator(value)
      while(valueIterator.hasNext) {
        val v = valueIterator.next()
        valueWriter.write(v, tokenWriter)
      }

      tokenWriter.writeArrayEnd()
    }
  }
}

private[tethys] trait LowPriorityJsonWriters {
  implicit final def lowPriorityWriter[A](implicit lowPriorityInstance: LowPriorityInstance[JsonObjectWriter[A]]): JsonWriter[A] = {
    lowPriorityInstance.instance
  }
}
