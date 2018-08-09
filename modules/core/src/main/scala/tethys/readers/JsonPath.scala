package tethys.readers

import tethys.JsonReader
import tethys.readers.JsonPath.JsonObjectPart
import tethys.readers.instances.SingleValueObjectReader

import scala.language.dynamics


sealed trait JsonPath extends Dynamic {
  def as[A](implicit reader: JsonReader[A]): JsonReader[A] = buildReader(reader)
  def array()

  def selectDynamic(name: String): JsonObjectPart = new JsonObjectPart(name, this)
  def applyDynamic(index: Int): JsonObjectPart = ???

  protected def buildReader[A](inner: JsonReader[A]): JsonReader[A]
}


object JsonPath extends JsonPath {

  override protected def buildReader[A](inner: JsonReader[A]): JsonReader[A] = inner

  final class JsonObjectPart(name: String, outer: JsonPath) extends JsonPath {
    override protected def buildReader[A](inner: JsonReader[A]): JsonReader[A] = {
      outer.buildReader(new SingleValueObjectReader[A](name, inner))
    }
  }


}
