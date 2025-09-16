package tethys.readers.instances

import tethys.JsonReader
import tethys.commons.Token
import tethys.readers.tokens.TokenIterator
import tethys.readers.FieldName

import scala.collection.Factory
import scala.language.higherKinds

private[tethys] trait IterableReaders extends LowPriorityJsonReaders {
  implicit def iterableReader[A, C[X] <: Iterable[X]](implicit
      jsonReader: JsonReader[A],
      from: Factory[A, C[A]]
  ): JsonReader[C[A]] = new JsonReader[C[A]] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName) = {
      val builder = from.newBuilder
      it.nextToken()
      var idx = 0
      while (it.currentToken() != Token.ArrayEndToken) {
        builder.addOne(jsonReader.read(it)(fieldName.appendArrayIndex(idx)))
        idx += 1
      }
      it.nextToken()
      builder.result()
    }
  }
}
