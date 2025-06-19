package tethys.catsInstances

import cats.data._
import tethys.JsonReader
import tethys.JsonReader.iterableReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

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
      override def defaultValue: NonEmptyList[T] = {
        given FieldName = FieldName("[defaultValue]")
        ReaderError.wrongJson("List is empty and can't be converted to NonEmptyList (in default value)")
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
      override def defaultValue: NonEmptyVector[T] = {
        given FieldName = FieldName("[defaultValue]")
        ReaderError.wrongJson("Vector is empty and can't be converted to NonEmptyVector (in default value)")
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
      override def defaultValue: NonEmptyChain[T] = {
        given FieldName = FieldName("[defaultValue]")
        ReaderError.wrongJson("Chain is empty and can't be converted to NonEmptyChain (in default value)")
      }
    }

}
