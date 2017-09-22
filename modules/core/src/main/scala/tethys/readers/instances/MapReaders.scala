package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

private[tethys] trait MapReaders extends LowPriorityMapReaders {
  implicit def shortMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                        keyReader: KeyReader[K],
                                                                        cbf: CanBuildFrom[Nothing, (K, Short), M[K, Short]],
                                                                        ct: ClassTag[M[K, Short]]): JsonReader[M[K, Short]] = {
    new MapReader[K, Short, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, Short), M[K, Short]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.ShortJsonReader.read(it)
      }
    }
  }

  implicit def intMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                      keyReader: KeyReader[K],
                                                                      cbf: CanBuildFrom[Nothing, (K, Int), M[K, Int]],
                                                                      ct: ClassTag[M[K, Int]]): JsonReader[M[K, Int]] = {
    new MapReader[K, Int, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, Int), M[K, Int]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.IntJsonReader.read(it)
      }
    }
  }

  implicit def longMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                       keyReader: KeyReader[K],
                                                                       cbf: CanBuildFrom[Nothing, (K, Long), M[K, Long]],
                                                                       ct: ClassTag[M[K, Long]]): JsonReader[M[K, Long]] = {
    new MapReader[K, Long, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, Long), M[K, Long]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.LongJsonReader.read(it)
      }
    }
  }

  implicit def floatMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                        keyReader: KeyReader[K],
                                                                        cbf: CanBuildFrom[Nothing, (K, Float), M[K, Float]],
                                                                        ct: ClassTag[M[K, Float]]): JsonReader[M[K, Float]] = {
    new MapReader[K, Float, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, Float), M[K, Float]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.FloatJsonReader.read(it)
      }
    }
  }

  implicit def doubleMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                         keyReader: KeyReader[K],
                                                                         cbf: CanBuildFrom[Nothing, (K, Double), M[K, Double]],
                                                                         ct: ClassTag[M[K, Double]]): JsonReader[M[K, Double]] = {
    new MapReader[K, Double, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, Double), M[K, Double]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.DoubleJsonReader.read(it)
      }
    }
  }

  implicit def booleanMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                          keyReader: KeyReader[K],
                                                                          cbf: CanBuildFrom[Nothing, (K, Boolean), M[K, Boolean]],
                                                                          ct: ClassTag[M[K, Boolean]]): JsonReader[M[K, Boolean]] = {
    new MapReader[K, Boolean, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, Boolean), M[K, Boolean]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.BooleanJsonReader.read(it)
      }
    }
  }
}

private[tethys] trait LowPriorityMapReaders extends IterableReaders {

  implicit def mapReader[K, A, M[X, Y] <: scala.collection.Map[X, Y]](implicit
                                                                      keyReader: KeyReader[K],
                                                                      jsonReader: JsonReader[A],
                                                                      cbf: CanBuildFrom[Nothing, (K, A), M[K, A]],
                                                                      ct: ClassTag[M[K, A]]): JsonReader[M[K, A]] = {
    new MapReader[K, A, M] {
      override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[(K, A), M[K, A]], key: K)(implicit fieldName: FieldName): Unit = {
        builder += key -> jsonReader.read(it)
      }
    }
  }

  protected abstract class MapReader[K, A, M[_, _]](implicit
                                                    keyReader: KeyReader[K],
                                                    cbf: CanBuildFrom[Nothing, (K, A), M[K, A]],
                                                    ct: ClassTag[M[K, A]]
                                                   ) extends JsonReader[M[K, A]] {

    protected def appendBuilder(it: TokenIterator,
                                builder: mutable.Builder[(K, A), M[K, A]],
                                key: K)(implicit fieldName: FieldName): Unit

    override def read(it: TokenIterator)(implicit fieldName: FieldName): M[K, A] = {
      if (it.currentToken().isObjectStart) recRead(it.next(), cbf())(fieldName)
      else ReaderError.wrongType[M[K, A]]
    }

    @tailrec
    private def recRead(it: TokenIterator, builder: mutable.Builder[(K, A), M[K, A]])
                       (fieldName: FieldName): M[K, A] = {
      it.currentToken() match {
        case token if token.isObjectEnd =>
          it.nextToken()
          builder.result()
        case token if token.isFieldName =>
          val name = it.fieldName()
          appendBuilder(it.next(), builder, keyReader.read(name))(fieldName.appendFieldName(name))
          recRead(it, builder)(fieldName)

        case _ => ReaderError.wrongJson(fieldName)
      }
    }
  }

}
