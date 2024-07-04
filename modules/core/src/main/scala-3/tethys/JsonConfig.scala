package tethys

sealed trait JsonConfig[A]:
  def discriminateBy[Field](select: A => Field): JsonConfig[A]


object JsonConfig:
  @scala.annotation.compileTimeOnly("Config must be an inlined given or provided directly to 'derived'")
  def apply[A]: JsonConfig[A] =
    throw IllegalStateException("Config must be an inlined given or provided directly to 'derived'")