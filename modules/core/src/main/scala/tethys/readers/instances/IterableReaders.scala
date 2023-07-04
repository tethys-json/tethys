package tethys.readers.instances

import tethys.JsonReader
import tethys.compat.CollectionBuilder
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, ReaderError}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds

private[tethys] trait IterableReaders extends LowPriorityIterableReaders {

  implicit def shortIterableReader[C[X] <: Iterable[X]](implicit
                                                           cb: CollectionBuilder[Short, C[Short]]): JsonReader[C[Short]] = new TraversableReader[Short, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Short, C[Short]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.ShortJsonReader.read(it)
    }
  }

  implicit def intIterableReader[C[X] <: Iterable[X]](implicit
                                                         cb: CollectionBuilder[Int, C[Int]]): JsonReader[C[Int]] = new TraversableReader[Int, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Int, C[Int]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.IntJsonReader.read(it)
    }
  }

  implicit def longIterableReader[C[X] <: Iterable[X]](implicit
                                                          cb: CollectionBuilder[Long, C[Long]]): JsonReader[C[Long]] = new TraversableReader[Long, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Long, C[Long]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.LongJsonReader.read(it)
    }
  }

  implicit def floatIterableReader[C[X] <: Iterable[X]](implicit
                                                           cb: CollectionBuilder[Float, C[Float]]): JsonReader[C[Float]] = new TraversableReader[Float, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Float, C[Float]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.FloatJsonReader.read(it)
    }
  }

  implicit def doubleIterableReader[C[X] <: Iterable[X]](implicit
                                                            cb: CollectionBuilder[Double, C[Double]]): JsonReader[C[Double]] = new TraversableReader[Double, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Double, C[Double]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.DoubleJsonReader.read(it)
    }
  }

  implicit def booleanIterableReader[C[X] <: Iterable[X]](implicit
                                                             cb: CollectionBuilder[Boolean, C[Boolean]]): JsonReader[C[Boolean]] = new TraversableReader[Boolean, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Boolean, C[Boolean]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.BooleanJsonReader.read(it)
    }
  }
}

private[tethys] trait LowPriorityIterableReaders extends LowPriorityJsonReaders {

  implicit def iterableReader[A, C[X] <: Iterable[X]](implicit
                                                      jsonReader: JsonReader[A],
                                                      collectionBuilder: CollectionBuilder[A, C[A]]): JsonReader[C[A]] = new TraversableReader[A, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[A, C[A]])(implicit fieldName: FieldName): Unit = {
      builder += jsonReader.read(it)
    }
  }

  protected abstract class TraversableReader[A, C[X] <: Iterable[X]](implicit
                                                                     cb: CollectionBuilder[A, C[A]]) extends JsonReader[C[A]] {
    protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[A, C[A]])(implicit fieldName: FieldName): Unit

    override def read(it: TokenIterator)(implicit fieldName: FieldName): C[A] = {
      if (it.currentToken().isArrayStart) recRead(0, it.next(), cb.newBuilder)
      else ReaderError.wrongJson(s"Expected array start but found: ${it.currentToken()}")
    }

    @tailrec
    private def recRead(i: Int, it: TokenIterator, builder: mutable.Builder[A, C[A]])
                       (implicit fieldName: FieldName): C[A] = {
      it.currentToken() match {
        case token if token.isEmpty => ReaderError.wrongJson("Unexpected end of input")
        case token if token.isArrayEnd =>
          it.nextToken()
          builder.result()
        case _ =>
          appendBuilder(it, builder)(fieldName.appendArrayIndex(i))
          recRead(i + 1, it, builder)
      }
    }
  }
}
