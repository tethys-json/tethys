package tethys.readers

import tethys.readers.JsonReaderDefaultValue.ReaderDefaultValue

import scala.annotation.implicitNotFound
import scala.annotation.StaticAnnotation

//@implicitNotFound("Missing JsonReaderDefaultValue[${A}]")
trait JsonReaderDefaultValue[A] {
  def defaultValue: Any
}

object JsonReaderDefaultValue extends LowPriorityDefaultValue {
  def apply[A](implicit
      dv: JsonReaderDefaultValue[A]
  ): JsonReaderDefaultValue[A] = dv

  case class JsonReaderDefaultValueImpl[A](defaultValue: A)
      extends JsonReaderDefaultValue[A]

  def apply[A](value: A): JsonReaderDefaultValue[A] =
    JsonReaderDefaultValueImpl(value)

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

  object Implicits {
    implicit lazy val byteDefaultValue: JsonReaderDefaultValue[Byte] =
      JsonReaderDefaultValueImpl(0)
    implicit lazy val charDefaultValue: JsonReaderDefaultValue[Byte] =
      JsonReaderDefaultValueImpl('\u0000')
    implicit lazy val shortDefaultValue: JsonReaderDefaultValue[Short] =
      JsonReaderDefaultValueImpl(0)
    implicit lazy val intDefaultValue: JsonReaderDefaultValue[Int] =
      JsonReaderDefaultValueImpl(0)
    implicit lazy val longDefaultValue: JsonReaderDefaultValue[Long] =
      JsonReaderDefaultValueImpl(0L)
    implicit lazy val stringDefaultValue: JsonReaderDefaultValue[String] =
      JsonReaderDefaultValueImpl("")
    implicit lazy val booleanDefaultValue: JsonReaderDefaultValue[Boolean] =
      JsonReaderDefaultValueImpl(false)
    implicit def seqDefaultValue[T]: JsonReaderDefaultValue[Seq[T]] =
      JsonReaderDefaultValueImpl(Seq())
    implicit def listDefaultValue[T]: JsonReaderDefaultValue[List[T]] =
      JsonReaderDefaultValueImpl(List())
    implicit lazy val floatDefaultValue: JsonReaderDefaultValue[Float] =
      JsonReaderDefaultValueImpl(0.0)
    implicit lazy val doubleDefaultValue: JsonReaderDefaultValue[Double] =
      JsonReaderDefaultValueImpl(0.0)
  }
  object AnyRefImplicit {
    implicit def anyRefNullDefaultValue[A <: AnyRef]
        : JsonReaderDefaultValue[A] = JsonReaderDefaultValue(null)
  }
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
