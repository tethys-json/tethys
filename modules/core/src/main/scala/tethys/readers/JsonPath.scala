package tethys.readers

import tethys.JsonReader
import tethys.readers.JsonPath.{JsonArrayPart, JsonObjectPart}
import tethys.readers.instances.{SingleArrayIndexReader, SingleValueObjectReader}

import scala.language.{dynamics, higherKinds}


sealed trait JsonPath extends Dynamic {
  type This >: this.type <: JsonPath
  type Inner <: JsonPath
  type Wrap[A]

  def as[A](implicit reader: JsonReader[A]): JsonReader[Wrap[A]] = buildReader(reader)

  def array: JsonArrayPart[This] = new JsonArrayPart[This](this)

  def selectDynamic(name: String): JsonObjectPart[This] = new JsonObjectPart[This](name, this, strict = true)

  protected def buildReader[A](inner: JsonReader[A]): JsonReader[Wrap[A]]
}


object JsonPath extends JsonPath {
  type This = JsonPath.type
  type Inner = Nothing
  type Wrap[A] = A

  override protected def buildReader[A](inner: JsonReader[A]): JsonReader[A] = inner

  final class JsonObjectPart[JP <: JsonPath](name: String, outer: JP, strict: Boolean) extends JsonPath {
    type This = JsonObjectPart[JP]
    type Inner = JP
    type Wrap[A] = JP#Wrap[A]

    def ? : JsonObjectOptPart[JP] = new JsonObjectOptPart[JP](name, outer)

    override protected def buildReader[A](inner: JsonReader[A]): JsonReader[Wrap[A]] = {
      outer.buildReader(new SingleValueObjectReader[A](name, inner, strict)).asInstanceOf[JsonReader[Wrap[A]]]
    }
  }

  final class JsonObjectOptPart[JP <: JsonPath](name: String, outer: JP) extends JsonPath {
    type This = JsonObjectOptPart[JP]
    type Inner = JP
    type Wrap[A] = JP#Wrap[Option[A]]

    def flat[FJP <: JsonPath](f: JsonObjectPart[JP] => JsonObjectOptPart[FJP])
                             (implicit ev: ContainPath[JsonObjectPart[JP], FJP]): JsonObjectOptPart[FJP] = {
      f(new JsonObjectPart[JP](name, outer, strict = false))
    }

    override protected def buildReader[A](inner: JsonReader[A]): JsonReader[Wrap[A]] = {
      outer.buildReader(new SingleValueObjectReader[Option[A]](name, JsonReader.optionReader[A](inner), strict = false)).asInstanceOf[JsonReader[Wrap[A]]]
    }
  }

  final class JsonArrayPart[JP <: JsonPath](outer: JP) extends JsonPath {
    type This = JsonArrayPart[JP]
    type Inner = JP
    type Wrap[A] = JP#Wrap[List[A]]

    def apply(pos: Int): JsonArrayElementPart[JP] = new JsonArrayElementPart[JP](pos, outer)

    override protected def buildReader[A](inner: JsonReader[A]): JsonReader[Wrap[A]] = {
      outer.buildReader(JsonReader.iterableReaderFor[List](inner)).asInstanceOf[JsonReader[Wrap[A]]]
    }
  }

  final class JsonArrayElementPart[JP <: JsonPath](pos: Int, outer: JP) extends JsonPath {
    type This = JsonArrayElementPart[JP]
    type Inner = JP
    type Wrap[A] = JP#Wrap[A]

    override protected def buildReader[A](inner: JsonReader[A]): JsonReader[JP#Wrap[A]] = {
      outer.buildReader(new SingleArrayIndexReader[A](pos, inner)).asInstanceOf[JsonReader[Wrap[A]]]
    }
  }

  def test: JsonReader[List[Option[Int]]] = JsonPath.foo.?.flat(_.array.bar.baz.?).as[Int]

  sealed trait ContainPath[A <: JsonPath, B <: JsonPath]
  object ContainPath extends LowPriorContainPath {
    implicit def eqPaths[A <: JsonPath, B <: JsonPath](implicit ev: A =:= B): ContainPath[A, B] = null
  }

  trait LowPriorContainPath {
    implicit def unwrapPaths[A <: JsonPath, B <: JsonPath](implicit containPath: ContainPath[A, B#Inner]): ContainPath[A, B] = null
  }

  sealed trait NotContainsPart[A <: JsonPath, B[_] <: JsonPath]
  object NotContainsPart {

  }
}
