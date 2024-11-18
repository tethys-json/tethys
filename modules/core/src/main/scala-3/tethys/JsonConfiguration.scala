package tethys

trait JsonConfiguration[-A]:
  def fieldStyle(fieldStyle: FieldStyle): JsonConfiguration[A]

  def strict: JsonConfiguration[A]


object JsonConfiguration:
  @scala.annotation.compileTimeOnly("JsonConfiguration should be declared as inline given")
  def apply[A]: JsonConfiguration[A] = throw IllegalAccessException()

