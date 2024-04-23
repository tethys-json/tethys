package tethys.cats.writers

import cats.Order
import cats.collections.{
  AvlSet,
  BitSet,
  Dequeue,
  HashMap,
  HashSet,
  Heap,
  PairingHeap,
  TreeList
}
import cats.data._
import tethys.JsonWriter
import tethys.writers.KeyWriter

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

  implicit val writerForBitSet: JsonWriter[BitSet] =
    JsonWriter[Set[Int]].contramap(_.toSet)

  implicit def writerForCatsDequeue[T: JsonWriter]: JsonWriter[Dequeue[T]] =
    JsonWriter[List[T]].contramap(_.toList)

  implicit def writerForCatsHashMap[K: KeyWriter, V: JsonWriter]
      : JsonWriter[HashMap[K, V]] =
    JsonWriter[Map[K, V]].contramap(_.toMap)

  implicit def writerForCatsHashSet[T: JsonWriter]: JsonWriter[HashSet[T]] =
    JsonWriter[Set[T]].contramap(_.toSet)

  implicit def writerForCatsHeap[T: JsonWriter: Order]: JsonWriter[Heap[T]] =
    JsonWriter[Seq[T]].contramap(_.toList)

  implicit def writerForPairingHeap[T: JsonWriter: Order]
      : JsonWriter[PairingHeap[T]] =
    JsonWriter[Seq[T]].contramap(_.toList)

  implicit def writerForTreeList[T: JsonWriter]: JsonWriter[TreeList[T]] =
    JsonWriter[List[T]].contramap(_.toList)

  implicit def writerForAvlSet[T: JsonWriter]: JsonWriter[AvlSet[T]] =
    JsonWriter[Seq[T]].contramap(_.toList)

}
