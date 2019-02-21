package tethys

import tethys.readers.instances.AllJsonReaders
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, JsonReaderBuilder}

import scala.language.higherKinds

trait JsonReader[@specialized(specializations) A] {
  self =>

  def read(it: TokenIterator)(implicit fieldName: FieldName): A

  def map[B](fun: A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B = fun(self.read(it))
  }

  def mapWithField[B](fun: FieldName => A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B = fun(fieldName)(self.read(it))
  }
}

object JsonReader extends AllJsonReaders {
  def apply[A](implicit jsonReader: JsonReader[A]): JsonReader[A] = jsonReader

  val builder: JsonReaderBuilder.type = JsonReaderBuilder
}
