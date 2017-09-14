package tethys.core.writers.instances

import java.math.{BigDecimal => JBigDecimal}

import tethys.core.writers.JsonWriter
import tethys.core.writers.token.TokenWriter

trait BasicWriters {

  implicit lazy val intJsonWriter: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val longJsonWriter: JsonWriter[Long] = new JsonWriter[Long] {
    override def write(value: Long, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val shortJsonWriter: JsonWriter[Short] = new JsonWriter[Short] {
    override def write(value: Short, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val doubleJsonWriter: JsonWriter[Double] = new JsonWriter[Double] {
    override def write(value: Double, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val floatJsonWriter: JsonWriter[Float] = new JsonWriter[Float] {
    override def write(value: Float, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val bigDecimalJsonWriter: JsonWriter[BigDecimal] = new JsonWriter[BigDecimal] {
    override def write(value: BigDecimal, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value.bigDecimal)
  }

  implicit lazy val booleanJsonWriter: JsonWriter[Boolean] = new JsonWriter[Boolean] {
    override def write(value: Boolean, tokenWriter: TokenWriter): Unit = tokenWriter.writeBoolean(value)
  }

  implicit lazy val stringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String, tokenWriter: TokenWriter): Unit = tokenWriter.writeString(value)
  }

  implicit lazy val charJsonWriter: JsonWriter[Char] = new JsonWriter[Char] {
    override def write(value: Char, tokenWriter: TokenWriter): Unit = tokenWriter.writeString(value.toString)
  }

  implicit lazy val javaIntJsonWriter: JsonWriter[java.lang.Integer] = new JsonWriter[java.lang.Integer] {
    override def write(value: java.lang.Integer, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaLongJsonWriter: JsonWriter[java.lang.Long] = new JsonWriter[java.lang.Long] {
    override def write(value: java.lang.Long, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaShortJsonWriter: JsonWriter[java.lang.Short] = new JsonWriter[java.lang.Short] {
    override def write(value: java.lang.Short, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaDoubleJsonWriter: JsonWriter[java.lang.Double] = new JsonWriter[java.lang.Double] {
    override def write(value: java.lang.Double, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaFloatJsonWriter: JsonWriter[java.lang.Float] = new JsonWriter[java.lang.Float] {
    override def write(value: java.lang.Float, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaBigDecimalJsonWriter: JsonWriter[JBigDecimal] = new JsonWriter[JBigDecimal] {
    override def write(value: JBigDecimal, tokenWriter: TokenWriter): Unit = tokenWriter.writeNumber(value)
  }

  implicit lazy val javaBooleanJsonWriter: JsonWriter[java.lang.Boolean] = new JsonWriter[java.lang.Boolean] {
    override def write(value: java.lang.Boolean, tokenWriter: TokenWriter): Unit = tokenWriter.writeBoolean(value)
  }

}
