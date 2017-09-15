package tethys.readers.tokens

trait Token {
  def isStringValue: Boolean

  def isNumberValue: Boolean

  def isBooleanValue: Boolean

  def isNullValue: Boolean

  def isFieldName: Boolean

  def isArrayStart: Boolean

  def isArrayEnd: Boolean

  def isObjectStart: Boolean

  def isObjectEnd: Boolean

  def isStructStart: Boolean = isObjectStart || isArrayStart

  def isStructEnd: Boolean = isObjectEnd || isArrayEnd

  def isEmpty: Boolean
}

object Token {
  case object Empty extends Token {
    override def isStringValue: Boolean = false
    override def isNumberValue: Boolean = false
    override def isBooleanValue: Boolean = false
    override def isNullValue: Boolean = false
    override def isFieldName: Boolean = false
    override def isArrayStart: Boolean = false
    override def isArrayEnd: Boolean = false
    override def isObjectStart: Boolean = false
    override def isObjectEnd: Boolean = false
    override def isStructStart: Boolean = false
    override def isStructEnd: Boolean = false
    override def isEmpty: Boolean = true
  }
}
