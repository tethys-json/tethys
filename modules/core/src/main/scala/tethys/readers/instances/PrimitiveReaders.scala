package tethys.readers.instances

import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

object PrimitiveReaders {
  object ShortJsonReader {
    val defaultValue: Option[Short] = None

    def read(it: TokenIterator)(implicit fieldName: FieldName): Short = {
      if(it.currentToken().isNumberValue) {
        val res = it.short()
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[Short]
      }
    }
  }

  object IntJsonReader {
    val defaultValue: Option[Int] = None

    def read(it: TokenIterator)(implicit fieldName: FieldName): Int = {
      if(it.currentToken().isNumberValue) {
        val res = it.int()
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[Int]
      }
    }
  }

  object LongJsonReader {
    val defaultValue: Option[Long] = None

    def read(it: TokenIterator)(implicit fieldName: FieldName): Long = {
      if(it.currentToken().isNumberValue) {
        val res = it.long()
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[Long]
      }
    }
  }

  object FloatJsonReader {
    val defaultValue: Option[Float] = None

    def read(it: TokenIterator)(implicit fieldName: FieldName): Float = {
      if(it.currentToken().isNumberValue) {
        val res = it.float()
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[Float]
      }
    }
  }

  object DoubleJsonReader {
    val defaultValue: Option[Double] = None

    def read(it: TokenIterator)(implicit fieldName: FieldName): Double = {
      if(it.currentToken().isNumberValue) {
        val res = it.double()
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[Double]
      }
    }
  }

  object BooleanJsonReader {
    val defaultValue: Option[Boolean] = None

    def read(it: TokenIterator)(implicit fieldName: FieldName): Boolean = {
      if(it.currentToken().isBooleanValue) {
        val res = it.boolean()
        it.nextToken()
        res
      } else {
        ReaderError.wrongType[Boolean]
      }
    }
  }
}
