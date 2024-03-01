package tethys.writers.instances

import tethys.JsonWriter
import tethys.writers.tokens.TokenWriter

trait AllJsonWriters extends OptionWriters with EitherWriters {
  implicit lazy val intWriter: JsonWriter[Int] = new JsonWriter[Int] {
    override def write(value: Int, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit lazy val longWriter: JsonWriter[Long] = new JsonWriter[Long] {
    override def write(value: Long, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit lazy val shortWriter: JsonWriter[Short] = new JsonWriter[Short] {
    override def write(value: Short, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit lazy val doubleWriter: JsonWriter[Double] = new JsonWriter[Double] {
    override def write(value: Double, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit lazy val floatWriter: JsonWriter[Float] = new JsonWriter[Float] {
    override def write(value: Float, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit lazy val bigDecimalWriter: JsonWriter[BigDecimal] =
    new JsonWriter[BigDecimal] {
      override def write(value: BigDecimal, tokenWriter: TokenWriter): Unit =
        tokenWriter.writeNumber(value)
    }

  implicit lazy val bigIntWriter: JsonWriter[BigInt] = new JsonWriter[BigInt] {
    override def write(value: BigInt, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNumber(value)
  }

  implicit lazy val booleanWriter: JsonWriter[Boolean] =
    new JsonWriter[Boolean] {
      override def write(value: Boolean, tokenWriter: TokenWriter): Unit =
        tokenWriter.writeBoolean(value)
    }

  implicit lazy val stringWriter: JsonWriter[String] = new JsonWriter[String] {
    override def write(value: String, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeString(value)
  }

  implicit lazy val javaIntWriter: JsonWriter[java.lang.Integer] =
    new JsonWriter[java.lang.Integer] {
      override def write(
          value: java.lang.Integer,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaLongWriter: JsonWriter[java.lang.Long] =
    new JsonWriter[java.lang.Long] {
      override def write(
          value: java.lang.Long,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaShortWriter: JsonWriter[java.lang.Short] =
    new JsonWriter[java.lang.Short] {
      override def write(
          value: java.lang.Short,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaDoubleWriter: JsonWriter[java.lang.Double] =
    new JsonWriter[java.lang.Double] {
      override def write(
          value: java.lang.Double,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaFloatWriter: JsonWriter[java.lang.Float] =
    new JsonWriter[java.lang.Float] {
      override def write(
          value: java.lang.Float,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaBigDecimalWriter: JsonWriter[java.math.BigDecimal] =
    new JsonWriter[java.math.BigDecimal] {
      override def write(
          value: java.math.BigDecimal,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaBigIntegerWriter: JsonWriter[java.math.BigInteger] =
    new JsonWriter[java.math.BigInteger] {
      override def write(
          value: java.math.BigInteger,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeNumber(value)
    }

  implicit lazy val javaBooleanWriter: JsonWriter[java.lang.Boolean] =
    new JsonWriter[java.lang.Boolean] {
      override def write(
          value: java.lang.Boolean,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeBoolean(value)
    }

  implicit lazy val uuidWriter: JsonWriter[java.util.UUID] =
    new JsonWriter[java.util.UUID] {
      override def write(
          value: java.util.UUID,
          tokenWriter: TokenWriter
      ): Unit = tokenWriter.writeString(value.toString)
    }

  implicit lazy val nullWriter: JsonWriter[Null] = new JsonWriter[Null] {
    override def write(value: Null, tokenWriter: TokenWriter): Unit =
      tokenWriter.writeNull()
  }
}
