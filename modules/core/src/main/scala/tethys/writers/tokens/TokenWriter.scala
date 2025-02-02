package tethys.writers.tokens

trait TokenWriter {
  def writeArrayStart(): this.type

  def writeArrayEnd(): this.type

  def writeObjectStart(): this.type

  def writeObjectEnd(): this.type

  def writeFieldName(name: String): this.type

  def writeString(v: String): this.type

  def writeNumber(v: Byte): this.type

  def writeNumber(v: Short): this.type

  def writeNumber(v: Int): this.type

  def writeNumber(v: Long): this.type

  def writeNumber(v: BigInt): this.type

  def writeNumber(v: Double): this.type

  def writeNumber(v: Float): this.type

  def writeNumber(v: BigDecimal): this.type

  def writeRawNumber(n: Number): this.type = n match {
    case jbd: java.math.BigDecimal => writeNumber(BigDecimal(jbd))
    case jint: java.lang.Integer   => writeNumber(jint.intValue())
    case jbyte: java.lang.Byte     => writeNumber(jbyte.longValue())
    case jshort: java.lang.Short   => writeNumber(jshort.longValue())
    case jlong: java.lang.Long     => writeNumber(jlong.longValue())
    case jbi: java.math.BigInteger => writeNumber(BigInt(jbi))
    case jfloat: java.lang.Float   => writeNumber(jfloat.floatValue())
    case jdouble: java.lang.Double => writeNumber(jdouble.doubleValue())
    case bd: BigDecimal            => writeNumber(bd)
    case bi: BigInt                => writeNumber(bi)
    case num                       => writeNumber(num.doubleValue())
  }

  def writeBoolean(v: Boolean): this.type

  def writeNull(): this.type

  @throws[UnsupportedOperationException]
  def writeRawJson(json: String): this.type
}

object TokenWriter {
  trait Flushing {
    def flush(): Unit
    def close(): Unit
  }
}
