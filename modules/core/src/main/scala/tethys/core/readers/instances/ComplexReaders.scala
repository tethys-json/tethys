package tethys.core.readers.instances

import tethys.core.readers._
import tethys.core.readers.tokens.TokenIterator

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

trait ComplexReaders extends LowPriorityComplexReaders {
  implicit def mapReader[K, A](implicit keyReader: KeyReader[K], jsonReader: JsonReader[A]): JsonReader[Map[K, A]] = new JsonReader[Map[K, A]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Map[K, A]] = {
      if(it.currentToken().isObjectStart) recRead(it, Map.newBuilder[K, A])
      else ReaderError.wrongType[Map[K, A]]
    }

    @tailrec
    private def recRead(it: TokenIterator, builder: mutable.Builder[(K, A), Map[K, A]])
                       (implicit fieldName: FieldName): Either[ReaderError, Map[K, A]] = {
      it.nextToken() match {
        case token if token.isObjectEnd => Right(builder.result())
        case token if token.isFieldName =>
          val name = it.fieldName()
          if(name.isEmpty) ReaderError.wrongJson
          else {
            ReaderError.catchNonFatal(keyReader.read(name.get)) match {
              case Right(key) =>
                jsonReader.read(it.next()) match {
                  case Right(value) =>
                    recRead(it, builder += key -> value)

                  case left => left.asInstanceOf[Either[ReaderError, Map[K, A]]]
                }
              case left => left.asInstanceOf[Either[ReaderError, Map[K, A]]]
            }
          }

        case _ => ReaderError.wrongJson
      }
    }
  }

  implicit def optionReader[A](implicit jsonReader: JsonReader[A]): JsonReader[Option[A]] = new JsonReader[Option[A]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, Option[A]] = {
      if(it.currentToken().isNullValue) Right(None)
      else jsonReader.read(it) match {
        case Right(value) => Right(Some(value))
        case left => left.asInstanceOf[Either[ReaderError, Option[A]]]
      }
    }
  }
}

private[readers] trait LowPriorityComplexReaders {
  implicit def genTraversableReader[A, C[X] <: Traversable[X]](implicit
                                                               jsonReader: JsonReader[A],
                                                               cbf: CanBuildFrom[Nothing, A, C[A]],
                                                               classTag: ClassTag[C[A]]
                                                              ): JsonReader[C[A]] = new JsonReader[C[A]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Either[ReaderError, C[A]] = {
      if(it.currentToken().isArrayStart) recRead(it, cbf())
      else ReaderError.wrongType[C[A]]
    }

    @tailrec
    private def recRead(it: TokenIterator, builder: mutable.Builder[A, C[A]])
                       (implicit fieldName: FieldName): Either[ReaderError, C[A]] = {
      it.nextToken() match {
        case token if token.isEmpty => ReaderError.wrongJson
        case token if token.isArrayEnd => Right(builder.result())
        case _ =>
          jsonReader.read(it) match {
            case Right(value) =>
              recRead(it, builder += value)

            case left => left.asInstanceOf[Either[ReaderError, C[A]]]
          }
      }
    }
  }
}