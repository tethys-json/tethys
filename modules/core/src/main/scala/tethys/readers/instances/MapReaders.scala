package tethys.readers.instances

import tethys.JsonReader
import tethys.commons.Token
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, KeyReader, ReaderError}

import scala.collection.Factory
import scala.language.higherKinds

private[tethys] trait MapReaders extends IterableReaders {
  implicit def mapReader[K, V, M[X, Y] <: scala.collection.Map[X, Y]](implicit
      keyReader: KeyReader[K],
      valueReader: JsonReader[V],
      factory: Factory[(K, V), M[K, V]]
  ): JsonReader[M[K, V]] = new JsonReader[M[K, V]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName) = {
      val builder = factory.newBuilder
      it.nextToken()
      while (it.currentToken() != Token.ObjectEndToken) {
        val name = it.fieldName()
        it.nextToken()
        builder.addOne(
          (
            keyReader.read(name),
            valueReader.read(it)(fieldName.appendFieldName(name))
          )
        )
      }
      it.nextToken()
      builder.result()
    }
  }
}
