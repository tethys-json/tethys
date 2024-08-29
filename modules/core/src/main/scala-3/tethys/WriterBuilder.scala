package tethys

import tethys.FieldStyle

sealed trait WriterBuilder[A]:
  import WriterBuilder.*

  def add(name: String): FunApply0[A, A]

  def remove[B](field: A => B): WriterBuilder[A]

  def rename[B](field: A => B)(rename: String): WriterBuilder[A]

  def update[B](field: A => B): FunApply[A, B] with WithRename[FunApply[A, B]]

  @deprecated("Use 'update' instead")
  def updatePartial[B](
      field: A => B
  ): FunApply[A, B] with WithRename[FunApply[A, B]]

  def fieldStyle(style: FieldStyle): WriterBuilder[A]

  @deprecated("Use tethys.FieldStyle instead")
  def fieldStyle(
      fieldStyle: tethys.derivation.builder.FieldStyle
  ): WriterBuilder[A]

object WriterBuilder:
  def apply[A](using
      mirror: scala.deriving.Mirror.ProductOf[A]
  ): WriterBuilder[A] =
    throw IllegalStateException(
      "Config must be an inlined given or provided directly to 'derived'"
    )

  sealed trait WithRename[Res]:
    def withRename(rename: String): Res

  sealed trait FunApply0[A, B]:
    def apply[C](fun: B => C): WriterBuilder[A]

  sealed trait FunApply[A, B] extends FunApply0[A, B]:
    def fromRoot[C](fun: A => C): WriterBuilder[A]
