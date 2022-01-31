package tethys.derivation.builder

import tethys.derivation.builder.WriterBuilder._

import scala.annotation.compileTimeOnly


/**
 * Created by eld0727 on 22.04.17.
 */

sealed trait WriterBuilder[A] {
  def remove[B](field: A => B): WriterBuilder[A]

  def rename[B](field: A => B)(rename: String): WriterBuilder[A]

  def update[B](field: A => B): FunApply[A, B] with WithRename[FunApply[A, B]]

  def updatePartial[B](field: A => B): PartialFunApply[A, B] with WithRename[PartialFunApply[A, B]]

  def add(name: String): FunApply[A, A]

  def fieldStyle(fieldStyle: FieldStyle): WriterBuilder[A]
}

object WriterBuilder {

//  @compileTimeOnly("WriterBuilder should be defined in describe, jsonWriter of jsonReader macro")
  def apply[A <: Product]: WriterBuilder[A] = throw new NotDescribedException

  sealed trait WithRename[Res] {
    def withRename(rename: String): Res
  }

  sealed trait FunApply[A, B] {
    def apply[C](fun: B => C): WriterBuilder[A]
    def fromRoot[C](fun: A => C): WriterBuilder[A]
  }

  sealed trait PartialFunApply[A, B] {
    def apply[C](partial: PartialFunction[B, C]): WriterBuilder[A]
    def fromRoot[C](partial: PartialFunction[A, C]): WriterBuilder[A]
  }
}



