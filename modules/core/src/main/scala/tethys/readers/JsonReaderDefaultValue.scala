package tethys.readers

import tethys.readers.JsonReaderDefaultValue.ReaderDefaultValue

import scala.annotation.implicitNotFound
import scala.annotation.StaticAnnotation

//@implicitNotFound("Missing JsonReaderDefaultValue[${A}]")
trait JsonReaderDefaultValue[A] {
  def defaultValue: Any
}

case class JsonReaderDefaultValueImpl[A](defaultValue: A) extends JsonReaderDefaultValue[A]

object JsonReaderDefaultValue extends LowPriorityDefaultValue {
  def apply[A](implicit
      dv: JsonReaderDefaultValue[A]
  ): JsonReaderDefaultValue[A] = dv

  // Allows easy access of default value in macro
  class ReaderDefaultValue(value: Any) extends StaticAnnotation

  @ReaderDefaultValue(None)
  class OptionDefaultValue[A] extends JsonReaderDefaultValue[Option[A]] {
    override def defaultValue: Any = None
  }
  private val optionInstance: OptionDefaultValue[Nothing] =
    new OptionDefaultValue[Nothing]
  implicit def optionDefaultValue[A]: OptionDefaultValue[A] =
    optionInstance.asInstanceOf[OptionDefaultValue[A]]
}

trait LowPriorityDefaultValue {
  @ReaderDefaultValue(null)
  class NoDefaultValue[A] extends JsonReaderDefaultValue[A] {
    override def defaultValue: Any = null
  }

  private val noDefaultValueInstance: NoDefaultValue[Nothing] =
    new NoDefaultValue[Nothing]
  def noDefaultValue[A]: NoDefaultValue[A] =
    noDefaultValueInstance.asInstanceOf[NoDefaultValue[A]]
}
