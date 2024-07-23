package tethys.derivation.builder

import java.util.regex.Pattern

@deprecated
enum FieldStyle {
  case Capitalize, Uncapitalize, LowerCase, UpperCase

  case KebabCase, LowerKebabCase, UpperKebabCase, CapitalizedKebabCase

  case SnakeCase, LowerSnakeCase, UpperSnakeCase, CapitalizedSnakeCase
}
