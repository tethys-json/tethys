package tethys.writers.instances

import tethys._
import tethys.writers.tokens.TokenWriter

import scala.language.higherKinds

private[tethys] trait IterableWriters extends LowPriorityJsonWriters {
  final implicit def iterableWriter[A, C[X] <: Iterable[X]](implicit
      valueWriter: JsonWriter[A]
  ): JsonWriter[C[A]] = new IterableWriter[A, C](valueWriter) {
    override def iterator(c: C[A]): Iterator[A] = c.iterator
  }

  abstract class IterableWriter[A, C[_]](valueWriter: JsonWriter[A])
      extends JsonWriter[C[A]] {
    def iterator(c: C[A]): Iterator[A]

    override def write(value: C[A], tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeArrayStart()

      val valueIterator = iterator(value)
      while (valueIterator.hasNext) {
        val v = valueIterator.next()
        valueWriter.write(v, tokenWriter)
      }

      tokenWriter.writeArrayEnd()
    }
  }
}
