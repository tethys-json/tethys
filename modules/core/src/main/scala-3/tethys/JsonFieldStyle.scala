package tethys

import java.util.regex.Pattern


enum JsonFieldStyle {
  case Capitalize, Uncapitalize, LowerCase, UpperCase
  
  case KebabCase, LowerKebabCase, UpperKebabCase, CapitalizedKebabCase

  case SnakeCase, LowerSnakeCase, UpperSnakeCase, CapitalizedSnakeCase
}

private[tethys]
object JsonFieldStyle:
  private val regexp1: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"
  private val snakeCase: String => String = splitName(_).mkString("_")
  private val kebabcase: String => String = splitName(_).mkString("-")
  private val capitalize: String => String = _.capitalize
  private val uncapitalize: String => String = (field: String) => field.updated(0, field.charAt(0).toLower)
  private val lowercase: String => String = _.toLowerCase()
  private val uppercase: String => String = _.toUpperCase()
  
  def applyStyle(string: String, style: JsonFieldStyle): String =
    style match
      case JsonFieldStyle.Capitalize => capitalize(string)
      case JsonFieldStyle.Uncapitalize => uncapitalize(string)
      case JsonFieldStyle.LowerCase => lowercase(string)
      case JsonFieldStyle.UpperCase => uppercase(string)
      
      case JsonFieldStyle.KebabCase => kebabcase(string)
      case JsonFieldStyle.LowerKebabCase => (kebabcase andThen lowercase)(string)
      case JsonFieldStyle.UpperKebabCase => (kebabcase andThen uppercase)(string)
      case JsonFieldStyle.CapitalizedKebabCase => (kebabcase andThen capitalize)(string)

      case JsonFieldStyle.SnakeCase => snakeCase(string)
      case JsonFieldStyle.LowerSnakeCase => (snakeCase andThen lowercase)(string)
      case JsonFieldStyle.UpperSnakeCase => (snakeCase andThen uppercase)(string)
      case JsonFieldStyle.CapitalizedSnakeCase => (snakeCase andThen capitalize)(string)


  private def splitName(name: String): List[String] =
    val first = JsonFieldStyle.regexp1.matcher(name).replaceAll(JsonFieldStyle.replacement)
    JsonFieldStyle.regexp2.matcher(first).replaceAll(JsonFieldStyle.replacement).split("_").toList
