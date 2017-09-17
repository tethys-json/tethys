package tethys.commons

trait Token {
  def isStringValue: Boolean = false

  def isNumberValue: Boolean = false

  def isBooleanValue: Boolean = false

  def isNullValue: Boolean = false

  def isFieldName: Boolean = false

  def isArrayStart: Boolean = false

  def isArrayEnd: Boolean = false

  def isObjectStart: Boolean = false

  def isObjectEnd: Boolean = false

  def isStructStart: Boolean = isObjectStart || isArrayStart

  def isStructEnd: Boolean = isObjectEnd || isArrayEnd

  def isEmpty: Boolean = false
}

object Token {
  case object StringValueToken extends Token {
    override def isStringValue: Boolean = true
  }
  case object NumberValueToken extends Token {
    override def isNumberValue: Boolean = true
  }
  case object BooleanValueToken extends Token {
    override def isBooleanValue: Boolean = true
  }
  case object NullValueToken extends Token {
    override def isNullValue: Boolean = true
  }
  case object FieldNameToken extends Token {
    override def isFieldName: Boolean = true
  }
  case object ArrayStartToken extends Token {
    override def isArrayStart: Boolean = true
  }
  case object ArrayEndToken extends Token {
    override def isArrayEnd: Boolean = true
  }
  case object ObjectStartToken extends Token {
    override def isObjectStart: Boolean = true
  }
  case object ObjectEndToken extends Token {
    override def isObjectEnd: Boolean = true
  }
  case object Empty extends Token {
    override def isEmpty: Boolean = true
  }
}
