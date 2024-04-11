package tethys.integration.cats.writers

import cats.data._
import tethys.JsonWriter

trait CatsWriters {
  implicit def writerForNev[T: JsonWriter]: JsonWriter[NonEmptyVector[T]] =
    JsonWriter[Vector[T]].contramap(_.toVector)

  implicit def writerForNel[T: JsonWriter]: JsonWriter[NonEmptyList[T]] =
    JsonWriter[List[T]].contramap(_.toList)

  implicit def writerForNes[T: JsonWriter]: JsonWriter[NonEmptySet[T]] =
    JsonWriter[Set[T]].contramap(_.toSortedSet)

  implicit def writerForChain[T: JsonWriter]: JsonWriter[Chain[T]] =
    JsonWriter[List[T]].contramap(_.toList)

  implicit def writerForNec[T: JsonWriter]: JsonWriter[NonEmptyChain[T]] =
    JsonWriter[Chain[T]].contramap(_.toChain)
}
