package tethys.readers.instances

import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator
import tethys.{JsonReader, specializations}

private[tethys] trait OptionReaders extends MapReaders {
  implicit def optionReader[@specialized(specializations) A](implicit jsonReader: JsonReader[A]): JsonReader[Option[A]] = new OptionJsonReader[A] {
    override protected def readSomeValue(it: TokenIterator)(implicit fieldName: FieldName): Option[A] = {
      Some(jsonReader.read(it))
    }
  }

  @specialized
  protected abstract class OptionJsonReader[@specialized(specializations) A] extends JsonReader[Option[A]] {

    protected def readSomeValue(it: TokenIterator)(implicit fieldName: FieldName): Option[A]

    override val defaultValue: Option[Option[A]] = Some(None)

    override def read(it: TokenIterator)(implicit fieldName: FieldName): Option[A] = {
      if(it.currentToken().isNullValue) {
        it.nextToken()
        None
      } else {
        readSomeValue(it)
      }
    }
  }
}
