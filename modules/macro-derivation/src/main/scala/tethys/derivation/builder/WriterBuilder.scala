package tethys.derivation.builder

import tethys.derivation.builder.WriterBuilder._


/**
  * Created by eld0727 on 22.04.17.
  */

sealed trait WriterBuilder[A] {
  def remove[B](field: A => B): WriterBuilder[A] = throw new NotDescribedException

  def update[B](field: A => B): FunApply[A, B] = throw new NotDescribedException

  def updatePartial[B](field: A => B): PartialFunApply[A, B] = throw new NotDescribedException

  def add(name: String): FunApply[A, A] = throw new NotDescribedException
}

object WriterBuilder {
  class NotDescribedException extends Exception("Definition should be in describe block", null)

  def apply[A <: Product](): WriterBuilder[A] = throw new NotDescribedException

  sealed trait FunApply[A, B] {
    def apply[C](fun: B => C): WriterBuilder[A]
  }

  sealed trait PartialFunApply[A, B] {
    def apply[C](partial: PartialFunction[B, C]): WriterBuilder[A]
  }
}



