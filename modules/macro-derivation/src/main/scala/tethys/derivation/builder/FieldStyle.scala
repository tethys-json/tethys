package tethys.derivation.builder

import java.util.regex.Pattern

import scala.annotation.StaticAnnotation

trait FieldStyle  { self =>
  def applyStyle(field: String): String

  def andThen(that: FieldStyle): FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = that.applyStyle(self.applyStyle(field))
  }

  def andThen(that: String => String): FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = that.apply(self.applyStyle(field))
  }

  def >>(that: FieldStyle): FieldStyle = andThen(that)
  def >>(that: String => String): FieldStyle = andThen(that)
}


object FieldStyle {

  def apply(fun: String => String): FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = fun(field)
  }

  class Ref(fieldStyle: FieldStyle) extends StaticAnnotation
  trait StyleReference extends FieldStyle {
    final override def applyStyle(field: String): String = throw new RuntimeException("StyleReference should not be used at runtime")
  }

  // Names transformations adopted from scala enumeratum
  private val regexp1: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"

  private def splitName(name: String): List[String] = {
    val first = regexp1.matcher(name).replaceAll(replacement)
    regexp2.matcher(first).replaceAll(replacement).split("_").toList
  }

  val snakecase: FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = splitName(field).mkString("_")
  }

  val lowercase: FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = field.toLowerCase()
  }

  val uppercase: FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = field.toUpperCase()
  }

  val kebabcase: FieldStyle = new FieldStyle {
    override def applyStyle(field: String): String = splitName(field).mkString("-")
  }

  val lowerSnakecase: FieldStyle = snakecase >> lowercase
  val upperSnakecase: FieldStyle = snakecase >> uppercase
  val lowerKebabcase: FieldStyle = kebabcase >> lowercase
  val upperKebabcase: FieldStyle = kebabcase >> uppercase
}
