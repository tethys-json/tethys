package tethys.cats.readers

import cats.{Hash, Order}
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
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}
import tethys.JsonReader
import tethys.JsonReader.iterableReader

trait CatsReaders {

  implicit def readerForNel[T: JsonReader]: JsonReader[NonEmptyList[T]] =
    new JsonReader[NonEmptyList[T]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): NonEmptyList[T] =
        NonEmptyList.fromList(JsonReader[List[T]].read(it)) match {
          case Some(value) => value
          case None =>
            ReaderError.wrongJson(
              s"List is empty and can't be converted to NonEmptyList"
            )
        }
    }

  implicit def readerForNev[T: JsonReader]: JsonReader[NonEmptyVector[T]] =
    new JsonReader[NonEmptyVector[T]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): NonEmptyVector[T] =
        NonEmptyVector.fromVector(JsonReader[Vector[T]].read(it)) match {
          case Some(value) => value
          case None =>
            ReaderError.wrongJson(
              s"Vector is empty and can't be converted to NonEmptyVector"
            )
        }
    }

  implicit def readerForChain[T: JsonReader]: JsonReader[Chain[T]] =
    JsonReader[Seq[T]].map(Chain.fromIterableOnce)

  implicit def readerForNec[T: JsonReader]: JsonReader[NonEmptyChain[T]] =
    new JsonReader[NonEmptyChain[T]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): NonEmptyChain[T] =
        NonEmptyChain.fromChain(JsonReader[Chain[T]].read(it)) match {
          case Some(value) => value
          case None =>
            ReaderError.wrongJson(
              s"Chain is empty and can't be converted to NonEmptyChain"
            )
        }
    }

  implicit val readerForBitSet: JsonReader[BitSet] =
    JsonReader[Seq[Int]].map(seq => BitSet(seq: _*))

  implicit def readerForCatsDequeue[T: JsonReader]: JsonReader[Dequeue[T]] =
    JsonReader[Seq[T]].map(seq => Dequeue(seq: _*))

  implicit def readerForCatsHashMap[K: KeyReader: Hash, V: JsonReader]
      : JsonReader[HashMap[K, V]] =
    JsonReader[Map[K, V]].map(m => HashMap(m.toSeq: _*))

  implicit def readerForCatsHashSet[T: JsonReader: Hash]
      : JsonReader[HashSet[T]] =
    JsonReader[Seq[T]].map(HashSet.fromSeq(_))

  implicit def readerForCatsHeap[T: JsonReader: Order]: JsonReader[Heap[T]] =
    JsonReader[Seq[T]].map(Heap.fromIterable(_))

  implicit def readerForPairingHeap[T: JsonReader: Order]
      : JsonReader[PairingHeap[T]] =
    JsonReader[Seq[T]].map(PairingHeap.fromIterable(_))

  implicit def readerForTreeList[T: JsonReader]: JsonReader[TreeList[T]] =
    JsonReader[List[T]].map(TreeList.fromList)

  implicit def readerForAvlSet[T: JsonReader: Order]: JsonReader[AvlSet[T]] =
    JsonReader[List[T]].map(AvlSet.fromList(_))

}
