package tethys

import tethys.commons.LowPriorityInstance
import tethys.readers.instances.{BasicReaders, ComplexReaders}
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

trait JsonReader[A] {
  self =>

  def read(it: TokenIterator)(implicit fieldName: FieldName): A

  def defaultValue: Option[A] = None

  def map[B](fun: A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B = fun(self.read(it))
  }
}

object JsonReader
  extends BasicReaders
    with ComplexReaders
    with LowPriorityJsonReaders {
  def apply[A](implicit jsonReader: JsonReader[A]): JsonReader[A] = jsonReader
}

private[tethys] trait LowPriorityJsonReaders {
  implicit final def lowPriorityReader[A](implicit lowPriorityInstance: LowPriorityInstance[JsonReader[A]]): JsonReader[A] = {
    lowPriorityInstance.instance
  }
}
