package tethys.core.writers.builder

import tethys.core.writers.builder.WriterBuilder._


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
  def apply[A <: Product](): WriterBuilder[A] = throw new NotDescribedException

  sealed trait FunApply[A, B] {
    def apply[C](fun: B => C): WriterBuilder[A]
  }

  sealed trait PartialFunApply[A, B] {
    def apply[C](partial: PartialFunction[B, C]): WriterBuilder[A]
  }
}



