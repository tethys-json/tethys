package tethys

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.TokenWriter

object JsonStreaming {

  trait JsonCursor[A] {
    def pull(): Option[A]
  }

  def withJsonCursor[A, B](
      it: TokenIterator^
  )(op: JsonCursor[A]^ => B)(implicit jsonReader: JsonReader[A]^): B = {
    openArrayCursor(it, jsonReader)(op)
  }

  private def openArrayCursor[A, B](
      it: TokenIterator^,
      jsonReader: JsonReader[A]^
  )(op: JsonCursor[A]^ => B): B = {
    implicit val rootFieldName: FieldName = FieldName()

    if (!it.currentToken().isArrayStart) {
      ReaderError.wrongJson(
        s"Expected array start but found: ${it.currentToken()}"
      )
    }

    it.next()

    final class ArrayCursor extends JsonCursor[A] {
      private var index: Int = 0
      private var finished: Boolean = false

      override def pull(): Option[A] = {
        if (finished) {
          None
        } else if (it.currentToken().isArrayEnd) {
          finished = true
          it.next()
          None
        } else {
          val value = jsonReader.read(it)(using
            rootFieldName.appendArrayIndex(index)
          )
          index += 1
          Some(value)
        }
      }
    }

    op(new ArrayCursor)
  }

  def foldJsonStream[A, S](
      it: TokenIterator^,
      zero: S
  )(f: (S, A) => S)(implicit jsonReader: JsonReader[A]^): S = {
    foldDecodedArray(it, zero, jsonReader)(f)
  }

  private def foldDecodedArray[A, S](
      it: TokenIterator^,
      zero: S,
      jsonReader: JsonReader[A]^
  )(f: (S, A) => S): S = {
    implicit val rootFieldName: FieldName = FieldName()

    if (!it.currentToken().isArrayStart) {
      ReaderError.wrongJson(
        s"Expected array start but found: ${it.currentToken()}"
      )
    }

    it.next()

    var state = zero
    var index = 0

    while (!it.currentToken().isArrayEnd) {
      val value = jsonReader.read(it)(using
        rootFieldName.appendArrayIndex(index)
      )
      state = f(state, value)
      index += 1
    }

    it.next()
    state
  }

  def foreachDecoded[A](
      it: TokenIterator^
  )(f: A => Unit)(implicit jsonReader: JsonReader[A]^): Unit = {
    foreachDecodedValues(it, jsonReader)(f)
  }

  private def foreachDecodedValues[A](
      it: TokenIterator^,
      jsonReader: JsonReader[A]^
  )(f: A => Unit): Unit = {
    foldDecodedArray[A, Unit](it, (), jsonReader) { (_, value) =>
      f(value)
      ()
    }
  }

  def streamValue(from: TokenIterator^, to: TokenWriter^)(implicit
      fieldName: FieldName
  ): Unit = writeCurrentValue(from, to)

  private def writeCurrentValue(it: TokenIterator^, writer: TokenWriter^)(
      implicit fieldName: FieldName
  ): Unit = {
    val token = it.currentToken()
    if (token.isArrayStart) writeArray(it, writer)
    else if (token.isObjectStart) writeObject(it, writer)
    else if (token.isStringValue) writer.writeString(it.string())
    else if (token.isNumberValue) writer.writeRawNumber(it.number())
    else if (token.isBooleanValue) writer.writeBoolean(it.boolean())
    else if (token.isNullValue) writer.writeNull()
    else ReaderError.wrongJson(s"Expects value start but $token found")
    it.next()
  }

  private def writeArray(it: TokenIterator^, writer: TokenWriter^)(implicit
      fieldName: FieldName
  ): Unit = {
    it.next()
    writer.writeArrayStart()
    var index: Int = 0
    while (!it.currentToken().isArrayEnd) {
      writeCurrentValue(it, writer)(fieldName.appendArrayIndex(index))
      index = index + 1
    }
    writer.writeArrayEnd()
  }

  private def writeObject(it: TokenIterator^, writer: TokenWriter^)(implicit
      fieldName: FieldName
  ): Unit = {
    it.next()
    writer.writeObjectStart()
    while (!it.currentToken().isObjectEnd) {
      val token = it.currentToken()
      if (token.isFieldName) {
        val name = it.fieldName()
        writer.writeFieldName(name)
        writeCurrentValue(it.next(), writer)(fieldName.appendFieldName(name))
      } else {
        ReaderError.wrongJson(s"Expects field name but $token found")
      }
    }
    writer.writeObjectEnd()
  }
}
