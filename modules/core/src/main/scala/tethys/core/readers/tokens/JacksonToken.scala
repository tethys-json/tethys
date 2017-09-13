package tethys.core.readers.tokens

import com.fasterxml.jackson.core.JsonToken

case class JacksonToken(token: JsonToken) extends Token {
  override def isStringValue: Boolean = token == JsonToken.VALUE_STRING

  override def isNumberValue: Boolean = token.isNumeric

  override def isBooleanValue: Boolean = token.isBoolean

  override def isNullValue = token == JsonToken.VALUE_NULL

  override def isFieldName: Boolean = token == JsonToken.FIELD_NAME

  override def isArrayStart: Boolean = token == JsonToken.START_ARRAY

  override def isArrayEnd: Boolean = token == JsonToken.END_ARRAY

  override def isObjectStart: Boolean = token == JsonToken.START_OBJECT

  override def isObjectEnd: Boolean = token == JsonToken.END_OBJECT

  override def isEmpty: Boolean = false
}
