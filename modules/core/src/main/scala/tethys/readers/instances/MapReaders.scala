package tethys.readers.instances

import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}
import tethys.{JsonReader, specializations}

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds
import scala.reflect.ClassTag

private[tethys] trait MapReaders extends IterableReaders {

  implicit def mapReader[K, @specialized(specializations) A, M[X, Y] <: scala.collection.Map[X, Y]](implicit
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

  protected abstract class MapReader[K, @specialized(specializations) A, M[_, _]](implicit
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
