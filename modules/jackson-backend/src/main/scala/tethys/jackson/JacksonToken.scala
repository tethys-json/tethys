package tethys.jackson

import com.fasterxml.jackson.core.JsonTokenId
import tethys.readers.tokens.Token

case class JacksonToken(tokenId: Int) extends Token {
  override def isStringValue: Boolean = tokenId == JsonTokenId.ID_STRING

  override def isNumberValue: Boolean = tokenId match {
    case JsonTokenId.ID_NUMBER_INT => true
    case JsonTokenId.ID_NUMBER_FLOAT => true
    case _ => false
  }

  override def isBooleanValue: Boolean = tokenId match {
    case JsonTokenId.ID_TRUE => true
    case JsonTokenId.ID_FALSE => true
    case _ => false
  }

  override def isNullValue: Boolean = tokenId == JsonTokenId.ID_NULL

  override def isFieldName: Boolean = tokenId == JsonTokenId.ID_FIELD_NAME

  override def isArrayStart: Boolean = tokenId == JsonTokenId.ID_START_ARRAY

  override def isArrayEnd: Boolean = tokenId == JsonTokenId.ID_END_ARRAY

  override def isObjectStart: Boolean = tokenId == JsonTokenId.ID_START_OBJECT

  override def isObjectEnd: Boolean = tokenId == JsonTokenId.ID_END_OBJECT

  override def isEmpty: Boolean = tokenId == JsonTokenId.ID_NO_TOKEN
}
