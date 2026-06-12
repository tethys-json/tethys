package tethys

import java.util.regex.Pattern

enum FieldStyle {
  case Capitalize, Uncapitalize, LowerCase, UpperCase

  case KebabCase, LowerKebabCase, UpperKebabCase, CapitalizedKebabCase

  case SnakeCase, LowerSnakeCase, UpperSnakeCase, CapitalizedSnakeCase
}

object FieldStyle:
  private val regexp1: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"
  private def snakeCase(field: String): String = splitName(field).mkString("_")
  private def kebabcase(field: String): String = splitName(field).mkString("-")
  private def capitalize(field: String): String = field.capitalize
  private def uncapitalize(field: String): String =
    field.updated(0, field.charAt(0).toLower)
  private def lowercase(field: String): String = field.toLowerCase()
  private def uppercase(field: String): String = field.toUpperCase()

  private[tethys] def applyStyle(string: String, style: FieldStyle): String =
    style match
      case FieldStyle.Capitalize   => capitalize(string)
      case FieldStyle.Uncapitalize => uncapitalize(string)
      case FieldStyle.LowerCase    => lowercase(string)
      case FieldStyle.UpperCase    => uppercase(string)

      case FieldStyle.KebabCase      => kebabcase(string)
      case FieldStyle.LowerKebabCase => lowercase(kebabcase(string))
      case FieldStyle.UpperKebabCase => uppercase(kebabcase(string))
      case FieldStyle.CapitalizedKebabCase =>
        capitalize(kebabcase(string))

      case FieldStyle.SnakeCase      => snakeCase(string)
      case FieldStyle.LowerSnakeCase => lowercase(snakeCase(string))
      case FieldStyle.UpperSnakeCase => uppercase(snakeCase(string))
      case FieldStyle.CapitalizedSnakeCase =>
        capitalize(snakeCase(string))

  private def splitName(name: String): List[String] =
    val first =
      FieldStyle.regexp1.matcher(name).replaceAll(FieldStyle.replacement)
    FieldStyle.regexp2
      .matcher(first)
      .replaceAll(FieldStyle.replacement)
      .split("_")
      .toList
