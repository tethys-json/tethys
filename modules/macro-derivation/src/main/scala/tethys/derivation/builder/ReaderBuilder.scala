package tethys.derivation.builder

import tethys.JsonReader

import scala.annotation.compileTimeOnly

sealed trait ReaderBuilder[A] {
  def extract[B](field: (A) => B): DependentFieldAs[A, B]
  def extractReader[B](field: (A) => B): DependentField0[A, JsonReader[_ <: B]]

  def fieldStyle(style: FieldStyle): ReaderBuilder[A]
  def strict: ReaderBuilder[A]
}

object ReaderBuilder {
  @compileTimeOnly("ReaderBuilder should be defined in describe block")
  def apply[A]: ReaderBuilder[A] = throw new NotDescribedException

  sealed trait AsSyntax[A, B, C] {
    def apply(fun: B => C): ReaderBuilder[A]
  }
}
