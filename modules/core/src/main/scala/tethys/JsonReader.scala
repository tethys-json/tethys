package tethys

import tethys.readers.instances.AllJsonReaders
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, JsonReaderBuilder}

import scala.language.higherKinds

trait JsonReader[@specialized(specializations) A] {
  self =>

  def read(it: TokenIterator)(implicit fieldName: FieldName): A

  def defaultValue: Option[A] = None

  def map[B](fun: A => B): JsonReader[B] = new JsonReader[B] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): B =
      fun(self.read(it))

    override def defaultValue: Option[B] = self.defaultValue.map(fun)
  }

  def mapWithField[B](fun: FieldName => A => B): JsonReader[B] =
    new JsonReader[B] {
      override def read(it: TokenIterator)(implicit fieldName: FieldName): B =
        fun(fieldName)(self.read(it))
      override def defaultValue: Option[B] =
        self.defaultValue.map(fun(FieldName("[defaultValue]")))
    }
}

object JsonReader extends AllJsonReaders with derivation.JsonReaderDerivation {
  def apply[A](implicit jsonReader: JsonReader[A]): JsonReader[A] = jsonReader

  val builder: JsonReaderBuilder.type = JsonReaderBuilder
}
