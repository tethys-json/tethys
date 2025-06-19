package tethys.readers.instances

import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

object PrimitiveReaders {
  object ByteJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Byte = {
      if (it.currentToken().isNumberValue) {
        val res = it.byte()
        it.nextToken()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected byte value but found: ${it.currentToken()}"
        )
      }
    }
    val defaultValue: Option[Byte] = None
  }

  object ShortJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Short = {
      if (it.currentToken().isNumberValue) {
        val res = it.short()
        it.nextToken()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected short value but found: ${it.currentToken()}"
        )
      }
    }
    val defaultValue: Option[Short] = None
  }

  object IntJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Int = {
      if (it.currentToken().isNumberValue) {
        val res = it.int()
        it.nextToken()
        res
      } else {
        error(it)
      }
    }

    val defaultValue: Option[Int] = None

    private def error(it: TokenIterator)(implicit fieldName: FieldName) = {
      ReaderError.wrongJson(
        s"Expected int value but found: ${it.currentToken()}"
      )
    }
  }

  object LongJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Long = {
      if (it.currentToken().isNumberValue) {
        val res = it.long()
        it.nextToken()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected long value but found: ${it.currentToken()}"
        )
      }
    }

    val defaultValue: Option[Long] = None
  }

  object FloatJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Float = {
      if (it.currentToken().isNumberValue) {
        val res = it.float()
        it.nextToken()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected float value but found: ${it.currentToken()}"
        )
      }
    }
    val defaultValue: Option[Float] = None
  }

  object DoubleJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Double = {
      if (it.currentToken().isNumberValue) {
        val res = it.double()
        it.nextToken()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected double value but found: ${it.currentToken()}"
        )
      }
    }
    val defaultValue: Option[Double] = None
  }

  object BooleanJsonReader {
    def read(it: TokenIterator)(implicit fieldName: FieldName): Boolean = {
      if (it.currentToken().isBooleanValue) {
        val res = it.boolean()
        it.nextToken()
        res
      } else {
        ReaderError.wrongJson(
          s"Expected boolean value but found: ${it.currentToken()}"
        )
      }
    }
    val defaultValue: Option[Boolean] = None
  }
}
