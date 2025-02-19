package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.TokenWriter

object JsonStreaming:

  def streamValue(from: TokenIterator, to: TokenWriter)(implicit
      fieldName: FieldName
  ): Unit = writeCurrentValue(from, to)

  private def writeCurrentValue(it: TokenIterator, writer: TokenWriter)(implicit
      fieldName: FieldName
  ): Unit =
    val token = it.currentToken()
    if token.isArrayStart then writeArray(it, writer)
    else if token.isObjectStart then writeObject(it, writer)
    else if token.isStringValue then writer.writeString(it.string())
    else if token.isNumberValue then writer.writeRawNumber(it.number())
    else if token.isBooleanValue then writer.writeBoolean(it.boolean())
    else if token.isNullValue then writer.writeNull()
    else ReaderError.wrongJson(s"Expects value start but $token found")
    it.next()

  private def writeArray(it: TokenIterator, writer: TokenWriter)(implicit
      fieldName: FieldName
  ): Unit =
    it.next()
    writer.writeArrayStart()
    var index: Int = 0
    while !it.currentToken().isArrayEnd do
      writeCurrentValue(it, writer)(fieldName.appendArrayIndex(index))
      index = index + 1
    writer.writeArrayEnd()

  private def writeObject(it: TokenIterator, writer: TokenWriter)(implicit
      fieldName: FieldName
  ): Unit =
    it.next()
    writer.writeObjectStart()
    while !it.currentToken().isObjectEnd do
      val token = it.currentToken()
      if token.isFieldName then
        val name = it.fieldName()
        writer.writeFieldName(name)
        writeCurrentValue(it.next(), writer)(fieldName.appendFieldName(name))
      else
        ReaderError.wrongJson(s"Expects field name but $token found")
    writer.writeObjectEnd()
