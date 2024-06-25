package tethys.derivation

import tethys.JsonFieldStyle

private[derivation]
trait JsonWriterConfiguration:
  type ProductConfig[A] = JsonWriterProductConfig[A]

  @scala.annotation.compileTimeOnly("Config must be an inlined given or provided directly to 'derived'")
  def configure[A](using mirror: scala.deriving.Mirror.ProductOf[A]): ProductConfig[A] =
    throw IllegalStateException("Config must be an inlined given or provided directly to 'derived'")


sealed trait JsonWriterProductConfig[A]:
  import JsonWriterProductConfig.*

  def add(name: String): JsonWriterProductConfig.FunApply0[A, A]

  def remove[B](field: A => B): JsonWriterProductConfig[A]

  def rename[B](field: A => B)(rename: String): JsonWriterProductConfig[A]

  def update[B](field: A => B): FunApply[A, B] with WithRename[FunApply[A, B]]

  def fieldStyle(style: JsonFieldStyle): JsonWriterProductConfig[A]


object JsonWriterProductConfig:

  sealed trait WithRename[Res]:
    def withRename(rename: String): Res

  sealed trait FunApply0[A, B]:
    def apply[C](fun: B => C): JsonWriterProductConfig[A]

  sealed trait FunApply[A, B] extends FunApply0[A, B]:
    def fromRoot[C](fun: A => C): JsonWriterProductConfig[A]


sealed trait JsonWriterSumConfig[A]:
  def fieldStyle(style: JsonFieldStyle): JsonWriterSumConfig[A]