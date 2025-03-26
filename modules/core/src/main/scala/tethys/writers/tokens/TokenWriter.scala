package tethys.writers.tokens


trait TokenWriter {

  /** Writes a JSON array start marker [ . */
  def writeArrayStart(): this.type

  /** Writes a JSON array end marker ] . */
  def writeArrayEnd(): this.type

  /** Writes a JSON object start marker { . */
  def writeObjectStart(): this.type

  /** Writes a JSON object end marker } . */
  def writeObjectEnd(): this.type

  /** Writes a `String` value as a JSON key.
    *
    * @param name
    *   the `String` value to write
    * @throws TokenWriterException
    *   if the provided string has an illegal surrogate pair
    */
  def writeFieldName(name: String): this.type

  /** Writes a `String` value as a JSON value.
    *
    * @param v
    *   the `String` value to write
    * @throws TokenWriterException
    *   if the provided string has an illegal surrogate pair
    */
  def writeString(v: String): this.type

  /** Writes a `Byte` value as a JSON value.
    *
    * @param v
    *   the `Byte` value to write
    */
  def writeNumber(v: Byte): this.type

  /** Writes a `Short` value as a JSON value.
    *
    * @param v
    *   the `Short` value to write
    */
  def writeNumber(v: Short): this.type

  /** Writes a `Int` value as a JSON value.
    *
    * @param v
    *   the `Int` value to write
    */
  def writeNumber(v: Int): this.type

  /** Writes a `Long` value as a JSON value.
    *
    * @param v
    *   the `Long` value to write
    */
  def writeNumber(v: Long): this.type

  /** Writes a `Double` value as a JSON value.
    *
    * @param v
    *   the `Double` value to write
    *
    * @throws TokenWriterException
    *   if the value is non-finite
    */
  def writeNumber(v: Double): this.type

  /** Writes a `Float` value as a JSON value.
    *
    * @param v
    *   the `Float` value to write
    *
    * @throws TokenWriterException
    *   if the value is non-finite
    */
  def writeNumber(v: Float): this.type

  /** Writes a `BigInt` value as a JSON value.
    *
    * @param v
    *   the `BigInt` value to write
    */
  def writeNumber(v: BigInt): this.type

  /** Writes a `BigDecimal` value as a JSON value.
    *
    * @param v
    *   the `BigDecimal` value to write
    */
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

  /** Writes a `Boolean` value as a JSON value.
    *
    * @param v
    *   the `Boolean` value to write
    */
  def writeBoolean(v: Boolean): this.type

  /** Writes a JSON `null` value. */
  def writeNull(): this.type

  @throws[UnsupportedOperationException]
  def writeRawJson(json: String): this.type

  def flush(): Unit

  def close(): Unit

  def result(): String
}
