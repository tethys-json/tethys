package tethys.cats.readers

import cats.data.*
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}
import tethys.JsonReader
import tethys.JsonReader.iterableReader

import scala.collection.immutable.SortedSet

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

  implicit def readerForNes[T: JsonReader: Ordering]: JsonReader[NonEmptySet[T]] =
    new JsonReader[NonEmptySet[T]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): NonEmptySet[T] =
        NonEmptySet.fromSet(
          SortedSet.from(JsonReader[Seq[T]].read(it))
        ) match {
          case Some(value) => value
          case None =>
            ReaderError.wrongJson(
              s"Seq is empty and can't be converted to NonEmptySet"
            )
        }
    }

}
