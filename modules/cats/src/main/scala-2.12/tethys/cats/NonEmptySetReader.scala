package tethys.cats

import cats.data.NonEmptySet
import tethys.JsonReader
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.collection.immutable.{Seq, SortedSet}

trait NonEmptySetReader {

  implicit def readerForNes[T: JsonReader: Ordering]
      : JsonReader[NonEmptySet[T]] =
    new JsonReader[NonEmptySet[T]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): NonEmptySet[T] =
        NonEmptySet.fromSet(SortedSet(JsonReader[Seq[T]].read(it): _*)) match {
          case Some(value) => value
          case None =>
            ReaderError.wrongJson(
              s"Seq is empty and can't be converted to NonEmptySet"
            )
        }
    }
}
