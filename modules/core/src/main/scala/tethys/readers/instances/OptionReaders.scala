package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator

private[tethys] trait OptionReaders extends LowPriorityOptionReaders {
  implicit lazy val shortOptionReader: JsonReader[Option[Short]] =
    new OptionJsonReader[Short] {
      override protected def readSomeValue(
          it: TokenIterator
      )(implicit fieldName: FieldName): Option[Short] = {
        Some(PrimitiveReaders.ShortJsonReader.read(it))
      }
    }

  implicit lazy val intOptionReader: JsonReader[Option[Int]] =
    new OptionJsonReader[Int] {
      override protected def readSomeValue(
          it: TokenIterator
      )(implicit fieldName: FieldName): Option[Int] = {
        Some(PrimitiveReaders.IntJsonReader.read(it))
      }
    }

  implicit lazy val longOptionReader: JsonReader[Option[Long]] =
    new OptionJsonReader[Long] {
      override protected def readSomeValue(
          it: TokenIterator
      )(implicit fieldName: FieldName): Option[Long] = {
        Some(PrimitiveReaders.LongJsonReader.read(it))
      }
    }

  implicit lazy val floatOptionReader: JsonReader[Option[Float]] =
    new OptionJsonReader[Float] {
      override protected def readSomeValue(
          it: TokenIterator
      )(implicit fieldName: FieldName): Option[Float] = {
        Some(PrimitiveReaders.FloatJsonReader.read(it))
      }
    }

  implicit lazy val doubleOptionReader: JsonReader[Option[Double]] =
    new OptionJsonReader[Double] {
      override protected def readSomeValue(
          it: TokenIterator
      )(implicit fieldName: FieldName): Option[Double] = {
        Some(PrimitiveReaders.DoubleJsonReader.read(it))
      }
    }

  implicit lazy val booleanOptionReader: JsonReader[Option[Boolean]] =
    new OptionJsonReader[Boolean] {
      override protected def readSomeValue(
          it: TokenIterator
      )(implicit fieldName: FieldName): Option[Boolean] = {
        Some(PrimitiveReaders.BooleanJsonReader.read(it))
      }
    }
}

private[tethys] trait LowPriorityOptionReaders extends MapReaders {
  implicit def optionReader[A](implicit
      jsonReader: JsonReader[A]
  ): JsonReader[Option[A]] = new OptionJsonReader[A] {
    override protected def readSomeValue(
        it: TokenIterator
    )(implicit fieldName: FieldName): Option[A] = {
      Some(jsonReader.read(it))
    }
  }

  @specialized
  protected abstract class OptionJsonReader[A] extends JsonReader[Option[A]] {

    protected def readSomeValue(it: TokenIterator)(implicit
        fieldName: FieldName
    ): Option[A]

    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): Option[A] = {
      if (it.currentToken().isNullValue) {
        it.nextToken()
        None
      } else {
        readSomeValue(it)
      }
    }
  }

}
