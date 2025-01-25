package tethys

trait JsonConfiguration:
  def fieldStyle(fieldStyle: FieldStyle): JsonConfiguration

  def strict: JsonConfiguration


object JsonConfiguration:
  @scala.annotation.compileTimeOnly("JsonConfiguration should be declared as inline given")
  def default: JsonConfiguration = throw IllegalAccessException()

