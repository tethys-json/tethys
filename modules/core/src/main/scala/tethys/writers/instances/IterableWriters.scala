package tethys.writers.instances

import tethys._
import tethys.writers.tokens.TokenWriter

import scala.language.higherKinds

private[tethys] trait IterableWriters extends LowPriorityJsonWriters {
  final implicit def iterableWriter[A, C[X] <: Iterable[X]](implicit
      valueWriter: JsonWriter[A]
  ): JsonWriter[C[A]] = new JsonWriter[C[A]] {
    override def write(value: C[A], tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeArrayStart()

      val valueIterator = value.iterator

      while (valueIterator.hasNext)
        valueWriter.write(valueIterator.next(), tokenWriter)

      tokenWriter.writeArrayEnd()
    }
  }
}
