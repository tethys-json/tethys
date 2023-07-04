package tethys.derivation.impl

import java.util.regex.Pattern

import tethys.derivation.builder.{FieldStyle => ConfigFieldStyle}

private[impl] trait FieldStyle  { self =>
  def applyStyle(field: String): String

  private def andThen(that: FieldStyle): FieldStyle = (field: String) => that.applyStyle(self.applyStyle(field))
  private def >>(that: FieldStyle): FieldStyle = andThen(that)
}


private[impl] object FieldStyle {
  def apply(fun: String => String): FieldStyle = fun(_)

  // Names transformations adopted from scala enumeratum
  private val regexp1: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"

  private def splitName(name: String): List[String] = {
    val first = regexp1.matcher(name).replaceAll(replacement)
    regexp2.matcher(first).replaceAll(replacement).split("_").toList
  }

  val snakecase: FieldStyle = splitName(_).mkString("_")
  val kebabcase: FieldStyle = splitName(_).mkString("-")

  val lowercase: FieldStyle = _.toLowerCase()
  val uppercase: FieldStyle = _.toUpperCase()

  val capitalize: FieldStyle = _.capitalize
  val uncapitalize: FieldStyle = (field: String) =>
    Character.toLowerCase(field.charAt(0)) + field.substring(1)

  val lowerSnakecase: FieldStyle = snakecase >> lowercase
  val upperSnakecase: FieldStyle = snakecase >> uppercase
  val capitalizedSnakecase: FieldStyle = snakecase >> capitalize
  val uncapitalizedSnakecase: FieldStyle = snakecase >> uncapitalize

  val lowerKebabcase: FieldStyle = kebabcase >> lowercase
  val upperKebabcase: FieldStyle = kebabcase >> uppercase
  val capitalizedKebabCase: FieldStyle = kebabcase >> capitalize
  val uncapitalizedKebabCase: FieldStyle = kebabcase >> uncapitalize
}
