package tethys.readers.instances

import tethys.JsonReader
import tethys.compat.CollectionBuilder
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private[tethys] trait MapReaders extends LowPriorityMapReaders {
  implicit def shortMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, Short), M[K, Short]]
  ): JsonReader[M[K, Short]] = {
    new MapReader[K, Short, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, Short), M[K, Short]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.ShortJsonReader.read(it)
      }
    }
  }

  implicit def intMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, Int), M[K, Int]]
  ): JsonReader[M[K, Int]] = {
    new MapReader[K, Int, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, Int), M[K, Int]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.IntJsonReader.read(it)
      }
    }
  }

  implicit def longMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, Long), M[K, Long]]
  ): JsonReader[M[K, Long]] = {
    new MapReader[K, Long, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, Long), M[K, Long]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.LongJsonReader.read(it)
      }
    }
  }

  implicit def floatMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, Float), M[K, Float]]
  ): JsonReader[M[K, Float]] = {
    new MapReader[K, Float, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, Float), M[K, Float]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.FloatJsonReader.read(it)
      }
    }
  }

  implicit def doubleMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](
      implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, Double), M[K, Double]]
  ): JsonReader[M[K, Double]] = {
    new MapReader[K, Double, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, Double), M[K, Double]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.DoubleJsonReader.read(it)
      }
    }
  }

  implicit def booleanMapReader[K, M[X, Y] <: scala.collection.Map[X, Y]](
      implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, Boolean), M[K, Boolean]]
  ): JsonReader[M[K, Boolean]] = {
    new MapReader[K, Boolean, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, Boolean), M[K, Boolean]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> PrimitiveReaders.BooleanJsonReader.read(it)
      }
    }
  }
}

private[tethys] trait LowPriorityMapReaders extends IterableReaders {

  implicit def mapReader[K, A, M[X, Y] <: scala.collection.Map[X, Y]](implicit
      keyReader: KeyReader[K],
      jsonReader: JsonReader[A],
      cb: CollectionBuilder[(K, A), M[K, A]]
  ): JsonReader[M[K, A]] = {
    new MapReader[K, A, M] {
      override protected def appendBuilder(
          it: TokenIterator,
          builder: mutable.Builder[(K, A), M[K, A]],
          key: K
      )(implicit fieldName: FieldName): Unit = {
        builder += key -> jsonReader.read(it)
      }
    }
  }

  protected abstract class MapReader[K, A, M[_, _]](implicit
      keyReader: KeyReader[K],
      cb: CollectionBuilder[(K, A), M[K, A]]
  ) extends JsonReader[M[K, A]] {

    protected def appendBuilder(
        it: TokenIterator,
        builder: mutable.Builder[(K, A), M[K, A]],
        key: K
    )(implicit fieldName: FieldName): Unit

    override def read(
        it: TokenIterator
    )(implicit fieldName: FieldName): M[K, A] = {
      if (it.currentToken().isObjectStart)
        recRead(it.next(), cb.newBuilder)(fieldName)
      else
        ReaderError.wrongJson(
          s"Expected object start but found: ${it.currentToken()}"
        )
    }

    @tailrec
    private def recRead(
        it: TokenIterator,
        builder: mutable.Builder[(K, A), M[K, A]]
    )(fieldName: FieldName): M[K, A] = {
      it.currentToken() match {
        case token if token.isObjectEnd =>
          it.nextToken()
          builder.result()
        case token if token.isFieldName =>
          val name = it.fieldName()
          val nextFieldName = fieldName.appendFieldName(name)
          appendBuilder(
            it.next(),
            builder,
            keyReader.read(name)(nextFieldName)
          )(nextFieldName)
          recRead(it, builder)(fieldName)

        case token =>
          ReaderError.wrongJson(
            s"Expect end of object or field name but '$token' found"
          )(fieldName)
      }
    }
  }

}
