package tethys.readers.instances

import tethys.readers.instances.IterableReaders.IterableForApply
import tethys.{JsonReader, specializations}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

private[tethys] trait IterableReaders extends LowPriorityIterableReaders {

  implicit def shortIterableReader[C[X] <: Traversable[X]](implicit
                                                         cbf: CanBuildFrom[Nothing, Short, C[Short]],
                                                         classTag: ClassTag[C[Short]]): JsonReader[C[Short]] = new TraversableReader[Short, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Short, C[Short]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.ShortJsonReader.read(it)
    }
  }

  implicit def intIterableReader[C[X] <: Traversable[X]](implicit
                                                         cbf: CanBuildFrom[Nothing, Int, C[Int]],
                                                         classTag: ClassTag[C[Int]]): JsonReader[C[Int]] = new TraversableReader[Int, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Int, C[Int]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.IntJsonReader.read(it)
    }
  }

  implicit def longIterableReader[C[X] <: Traversable[X]](implicit
                                                         cbf: CanBuildFrom[Nothing, Long, C[Long]],
                                                         classTag: ClassTag[C[Long]]): JsonReader[C[Long]] = new TraversableReader[Long, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Long, C[Long]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.LongJsonReader.read(it)
    }
  }

  implicit def floatIterableReader[C[X] <: Traversable[X]](implicit
                                                         cbf: CanBuildFrom[Nothing, Float, C[Float]],
                                                         classTag: ClassTag[C[Float]]): JsonReader[C[Float]] = new TraversableReader[Float, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Float, C[Float]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.FloatJsonReader.read(it)
    }
  }

  implicit def doubleIterableReader[C[X] <: Traversable[X]](implicit
                                                         cbf: CanBuildFrom[Nothing, Double, C[Double]],
                                                         classTag: ClassTag[C[Double]]): JsonReader[C[Double]] = new TraversableReader[Double, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Double, C[Double]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.DoubleJsonReader.read(it)
    }
  }

  implicit def booleanIterableReader[C[X] <: Traversable[X]](implicit
                                                         cbf: CanBuildFrom[Nothing, Boolean, C[Boolean]],
                                                         classTag: ClassTag[C[Boolean]]): JsonReader[C[Boolean]] = new TraversableReader[Boolean, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[Boolean, C[Boolean]])(implicit fieldName: FieldName): Unit = {
      builder += PrimitiveReaders.BooleanJsonReader.read(it)
    }
  }
}

private[tethys] trait LowPriorityIterableReaders extends LowPriorityJsonReaders {

  def iterableReaderFor[C[X] <: Traversable[X]]: IterableForApply[C] = new IterableForApply[C](())

  implicit def iterableReader[A, C[X] <: Traversable[X]](implicit
                                                         jsonReader: JsonReader[A],
                                                         cbf: CanBuildFrom[Nothing, A, C[A]],
                                                         classTag: ClassTag[C[A]]): JsonReader[C[A]] = new TraversableReader[A, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[A, C[A]])(implicit fieldName: FieldName): Unit = {
      builder += jsonReader.read(it)
    }
  }

  protected abstract class TraversableReader[A, C[X] <: Traversable[X]](implicit
                                                                        cbf: CanBuildFrom[Nothing, A, C[A]],
                                                                        classTag: ClassTag[C[A]]) extends JsonReader[C[A]] {
    protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[A, C[A]])(implicit fieldName: FieldName): Unit

    override def read(it: TokenIterator)(implicit fieldName: FieldName): C[A] = {
      if (it.currentToken().isArrayStart) recRead(0, it.next(), cbf())
      else ReaderError.wrongType[C[A]]
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

object IterableReaders {
  class IterableForApply[C[X] <: Traversable[X]](val u: Unit) extends AnyVal {
    def apply[A](jsonReader: JsonReader[A])
                (implicit
                 cbf: CanBuildFrom[Nothing, A, C[A]],
                 classTag: ClassTag[C[A]]): JsonReader[C[A]] = {
      JsonReader.iterableReader[A, C](jsonReader, cbf, classTag)
    }
  }
}