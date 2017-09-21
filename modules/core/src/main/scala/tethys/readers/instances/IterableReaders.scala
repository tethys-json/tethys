package tethys.readers.instances

import tethys.{JsonReader, specializations}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

private[tethys] trait IterableReaders extends LowPriorityJsonReaders {
  
  implicit def iterableReader[@specialized(specializations) A, C[X] <: Traversable[X]](implicit
                                                                                       jsonReader: JsonReader[A],
                                                                                       cbf: CanBuildFrom[Nothing, A, C[A]],
                                                                                       classTag: ClassTag[C[A]]): JsonReader[C[A]] = new TraversableReader[A, C] {
    override protected def appendBuilder(it: TokenIterator, builder: mutable.Builder[A, C[A]])(implicit fieldName: FieldName): Unit = {
      builder += jsonReader.read(it)
    }
  }

  protected abstract class TraversableReader[@specialized(tethys.specializations) A, C[X] <: Traversable[X]](implicit
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
        case token if token.isEmpty => ReaderError.wrongJson
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
