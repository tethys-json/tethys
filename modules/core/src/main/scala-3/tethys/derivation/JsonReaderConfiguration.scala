package tethys.derivation

import tethys.{JsonFieldStyle, JsonReader}

private[derivation]
trait JsonReaderConfiguration:

  type ProductConfig[A] = JsonReaderProductConfig[A]

  @scala.annotation.compileTimeOnly("Config must be an inlined given or provided directly to 'derived'")
  def configure[A](using mirror: scala.deriving.Mirror.ProductOf[A]): ProductConfig[A] =
    throw IllegalStateException("Config must be an inlined given or provided directly to 'derived'")

sealed trait JsonReaderProductConfig[A]:
  def extract[Field](field: A => Field): JsonReaderProductConfig.DependentFieldAs[A, Field]

  def extractReader[Field](field: A => Field): JsonReaderProductConfig.DependentField0[A, JsonReader[_ <: Field]]

  def fieldStyle(fieldStyle: JsonFieldStyle): JsonReaderProductConfig[A]

  def strict: JsonReaderProductConfig[A]


object JsonReaderProductConfig:

  sealed trait AsSyntax[A, B, C]:
    def apply(fun: B => C): JsonReaderProductConfig[A]

  sealed trait DependentFieldAs[A, B] extends DependentField0[A, B]:
    def as[C]: JsonReaderProductConfig.AsSyntax[A, C, B]

  sealed trait DependentField0[A, Field]:
    def apply(fun: () => Field): JsonReaderProductConfig[A]

    def from[B](f1: A => B): DependentField1[A, Field, B]

    def from[B](name: String): DependentField1[A, Field, B]


  sealed trait DependentField1[A, Field, OneCollected]:
    def apply(fun: OneCollected => Field): JsonReaderProductConfig[A]

    def and[B](f1: A => B): DependentFieldN[A, Field, OneCollected *: B *: EmptyTuple]

    def and[B](name: String): DependentFieldN[A, Field, OneCollected *: B *: EmptyTuple]

    def product
    (using mirror: scala.deriving.Mirror.ProductOf[Field])
    (using ev: OneCollected *: EmptyTuple =:= mirror.MirroredElemTypes): JsonReaderProductConfig[A]

  sealed trait DependentFieldN[A, Field, Collected <: NonEmptyTuple]:
    def apply(fun: Collected => Field): JsonReaderProductConfig[A]

    def and[B](f1: A => B): DependentFieldN[A, Field, Tuple.Append[Collected, B]]

    def and[B](name: String): DependentFieldN[A, Field, Tuple.Append[Collected, B]]

    def product
    (using mirror: scala.deriving.Mirror.ProductOf[Field])
    (using ev: Collected =:= mirror.MirroredElemTypes): JsonReaderProductConfig[A]