package tethys.readers.instances

import tethys.JsonReader
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator

import scala.reflect.ClassTag

class SelectingJsonReader[A, B: ClassTag](simpleJsonReader: SimpleJsonReader[A])(selector: PartialFunction[A, JsonReader[_ <: B]]) extends JsonReader[B] {

  override def read(it: TokenIterator)(implicit fieldName: FieldName): B = {
    val it1 = it.collectExpression()
    val it2 = it1.copy()
    selector(simpleJsonReader.read(it1)).read(it2)
  }
}
