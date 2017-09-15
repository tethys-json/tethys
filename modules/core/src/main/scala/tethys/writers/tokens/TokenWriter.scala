package tethys.writers.tokens

trait TokenWriter {
  def writeArrayStart(): this.type

  def writeArrayEnd(): this.type

  def writeObjectStart(): this.type

  def writeObjectEnd(): this.type

  def writeFieldName(name: String): this.type

  def writeString(v: String): this.type

  def writeNumber(v: Short): this.type

  def writeNumber(v: Int): this.type

  def writeNumber(v: Long): this.type

  def writeNumber(v: BigInt): this.type

  def writeNumber(v: Double): this.type

  def writeNumber(v: Float): this.type

  def writeNumber(v: BigDecimal): this.type

  def writeBoolean(v: Boolean): this.type

  def writeNull(): this.type

  def close(): Unit
}
