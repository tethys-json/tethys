package tethys

import tethys.{FieldStyle, JsonReader}

sealed trait ReaderBuilder[A]:
  def extract[Field](
      field: A => Field
  ): ReaderBuilder.DependentFieldExtract[A, Field]

  def extractReader[Field](
      field: A => Field
  ): ReaderBuilder.DependentField0[A, JsonReader[_ <: Field]]

  def fieldStyle(fieldStyle: FieldStyle): ReaderBuilder[A]

  @deprecated("Use tethys.FieldStyle instead")
  def fieldStyle(
      fieldStyle: tethys.derivation.builder.FieldStyle
  ): ReaderBuilder[A]

  def strict: ReaderBuilder[A]

object ReaderBuilder:
  def apply[A](using
      mirror: scala.deriving.Mirror.ProductOf[A]
  ): ReaderBuilder[A] =
    throw IllegalStateException(
      "Config must be an inlined given or provided directly to 'derived'"
    )

  sealed trait AsSyntax[A, B, C]:
    def apply(fun: B => C): ReaderBuilder[A]

  sealed trait DependentFieldExtract[A, B] extends DependentField0[A, B]:
    def as[C]: ReaderBuilder.AsSyntax[A, C, B]

    def withRename(newName: String): ReaderBuilder[A]

  sealed trait DependentField0[A, Field]:
    def apply(fun: () => Field): ReaderBuilder[A]

    def from[B](f1: A => B): DependentField1[A, Field, B]

    def from[B](name: String): DependentField1[A, Field, B]

  sealed trait DependentField1[A, Field, OneCollected]:
    def apply(fun: OneCollected => Field): ReaderBuilder[A]

    def and[B](
        f1: A => B
    ): DependentFieldN[A, Field, OneCollected *: B *: EmptyTuple]

    def and[B](
        name: String
    ): DependentFieldN[A, Field, OneCollected *: B *: EmptyTuple]

    def product(using mirror: scala.deriving.Mirror.ProductOf[Field])(using
        ev: OneCollected *: EmptyTuple =:= mirror.MirroredElemTypes
    ): ReaderBuilder[A]

  sealed trait DependentFieldN[A, Field, Collected <: NonEmptyTuple]:
    def apply(fun: Collected => Field): ReaderBuilder[A]

    def and[B](
        f1: A => B
    ): DependentFieldN[A, Field, Tuple.Append[Collected, B]]

    def and[B](
        name: String
    ): DependentFieldN[A, Field, Tuple.Append[Collected, B]]

    def product(using mirror: scala.deriving.Mirror.ProductOf[Field])(using
        ev: Collected =:= mirror.MirroredElemTypes
    ): ReaderBuilder[A]
