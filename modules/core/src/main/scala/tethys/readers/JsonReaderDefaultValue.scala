package tethys.readers

import tethys.readers.JsonReaderDefaultValue.ReaderDefaultValue

import scala.annotation.StaticAnnotation

sealed trait JsonReaderDefaultValue[+A] {
  def defaultValue: Any
}

object JsonReaderDefaultValue extends LowPriorityDefaultValue {
  def apply[A](implicit dv: JsonReaderDefaultValue[A]): JsonReaderDefaultValue[A] = dv

  //Allow easy access of it value in macro
  private[tethys] class ReaderDefaultValue(value: Any) extends StaticAnnotation

  @ReaderDefaultValue(None)
  implicit object OptionDefaultValue extends JsonReaderDefaultValue[Option[Nothing]] {
    override def defaultValue: Any = None
  }
}

trait LowPriorityDefaultValue {
  @ReaderDefaultValue(null)
  implicit object NoDefaultValue extends JsonReaderDefaultValue[Nothing] {
    override def defaultValue: Any = null
  }
}
