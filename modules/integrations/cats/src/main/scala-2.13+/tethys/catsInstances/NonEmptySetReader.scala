package tethys.catsInstances

import cats.data.NonEmptySet
import tethys.JsonReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

import scala.collection.immutable.SortedSet

trait NonEmptySetReader {

  implicit def readerForNes[T: JsonReader: Ordering]
      : JsonReader[NonEmptySet[T]] =
    new JsonReader[NonEmptySet[T]] {
      override def read(
          it: TokenIterator
      )(implicit fieldName: FieldName): NonEmptySet[T] =
        NonEmptySet.fromSet(SortedSet.from(JsonReader[Seq[T]].read(it))) match {
          case Some(value) => value
          case None =>
            ReaderError.wrongJson(
              s"Seq is empty and can't be converted to NonEmptySet"
            )
        }
    }
}
