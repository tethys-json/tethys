package tethys.derivation.builder

enum FieldStyle {
  case Capitalize
  case Uncapitalize

  case LowerCase
  case UpperCase

  case KebabCase
  case LowerKebabCase
  case UpperKebabCase
  case CapitalizedKebabCase
  case UncapitalizedKebabCase

  case SnakeCase
  case LowerSnakeCase
  case UpperSnakeCase
  case CapitalizedSnakeCase
  case UncapitalizedSnakeCase

  @deprecated("use FieldStyle.LowerCase")
  case lowercase
  @deprecated("use FieldStyle.UpperCase")
  case uppercase
  
  @deprecated("use FieldStyle.SnakeCase")
  case snakecase
  @deprecated("use FieldStyle.LowerSnakeCase")
  case lowerSnakecase
  @deprecated("use FieldStyle.UpperSnakeCase")
  case upperSnakecase

  @deprecated("use FieldStyle.KebabCase")
  case kebabcase
  @deprecated("use FieldStyle.LowerKebabCase")
  case lowerKebabcase
  @deprecated("use FieldStyle.UpperKebabCase")
  case upperKebabcase
}
