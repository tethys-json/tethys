package tethys

import java.util.regex.Pattern


enum FieldStyle {
  case Capitalize, Uncapitalize, LowerCase, UpperCase

  case KebabCase, LowerKebabCase, UpperKebabCase, CapitalizedKebabCase

  case SnakeCase, LowerSnakeCase, UpperSnakeCase, CapitalizedSnakeCase
}

private[tethys]
object FieldStyle:
  private val regexp1: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"
  private val snakeCase: String => String = splitName(_).mkString("_")
  private val kebabcase: String => String = splitName(_).mkString("-")
  private val capitalize: String => String = _.capitalize
  private val uncapitalize: String => String = (field: String) => field.updated(0, field.charAt(0).toLower)
  private val lowercase: String => String = _.toLowerCase()
  private val uppercase: String => String = _.toUpperCase()

  def applyStyle(string: String, style: FieldStyle): String =
    style match
      case FieldStyle.Capitalize => capitalize(string)
      case FieldStyle.Uncapitalize => uncapitalize(string)
      case FieldStyle.LowerCase => lowercase(string)
      case FieldStyle.UpperCase => uppercase(string)

      case FieldStyle.KebabCase => kebabcase(string)
      case FieldStyle.LowerKebabCase => (kebabcase andThen lowercase)(string)
      case FieldStyle.UpperKebabCase => (kebabcase andThen uppercase)(string)
      case FieldStyle.CapitalizedKebabCase => (kebabcase andThen capitalize)(string)

      case FieldStyle.SnakeCase => snakeCase(string)
      case FieldStyle.LowerSnakeCase => (snakeCase andThen lowercase)(string)
      case FieldStyle.UpperSnakeCase => (snakeCase andThen uppercase)(string)
      case FieldStyle.CapitalizedSnakeCase => (snakeCase andThen capitalize)(string)


  private def splitName(name: String): List[String] =
    val first = FieldStyle.regexp1.matcher(name).replaceAll(FieldStyle.replacement)
    FieldStyle.regexp2.matcher(first).replaceAll(FieldStyle.replacement).split("_").toList
