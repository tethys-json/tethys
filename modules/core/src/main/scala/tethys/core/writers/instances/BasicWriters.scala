package tethys.core.writers.instances

import java.math.{BigDecimal => JBigDecimal}

import com.fasterxml.jackson.core.JsonGenerator
import tethys.core.writers.JsonWriter

trait BasicWriters {

  implicit lazy val intJsonWriter: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val longJsonWriter: JsonWriter[Long] = new JsonWriter[Long] {
    override def write(value: Long, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val shortJsonWriter: JsonWriter[Short] = new JsonWriter[Short] {
    override def write(value: Short, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val doubleJsonWriter: JsonWriter[Double] = new JsonWriter[Double] {
    override def write(value: Double, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val floatJsonWriter: JsonWriter[Float] = new JsonWriter[Float] {
    override def write(value: Float, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val bigDecimalJsonWriter: JsonWriter[BigDecimal] = new JsonWriter[BigDecimal] {
    override def write(value: BigDecimal, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value.bigDecimal)
  }

  implicit lazy val booleanJsonWriter: JsonWriter[Boolean] = new JsonWriter[Boolean] {
    override def write(value: Boolean, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeBoolean(value)
  }

  implicit lazy val stringJsonWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeString(value)
  }

  implicit lazy val charJsonWriter: JsonWriter[Char] = new JsonWriter[Char] {
    override def write(value: Char, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeString(value.toString)
  }

  implicit lazy val javaIntJsonWriter: JsonWriter[java.lang.Integer] = new JsonWriter[java.lang.Integer] {
    override def write(value: java.lang.Integer, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val javaLongJsonWriter: JsonWriter[java.lang.Long] = new JsonWriter[java.lang.Long] {
    override def write(value: java.lang.Long, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val javaShortJsonWriter: JsonWriter[java.lang.Short] = new JsonWriter[java.lang.Short] {
    override def write(value: java.lang.Short, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val javaDoubleJsonWriter: JsonWriter[java.lang.Double] = new JsonWriter[java.lang.Double] {
    override def write(value: java.lang.Double, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val javaFloatJsonWriter: JsonWriter[java.lang.Float] = new JsonWriter[java.lang.Float] {
    override def write(value: java.lang.Float, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val javaBigDecimalJsonWriter: JsonWriter[JBigDecimal] = new JsonWriter[JBigDecimal] {
    override def write(value: JBigDecimal, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeNumber(value)
  }

  implicit lazy val javaBooleanJsonWriter: JsonWriter[java.lang.Boolean] = new JsonWriter[java.lang.Boolean] {
    override def write(value: java.lang.Boolean, jsonGenerator: JsonGenerator): Unit = jsonGenerator.writeBoolean(value)
  }

}
