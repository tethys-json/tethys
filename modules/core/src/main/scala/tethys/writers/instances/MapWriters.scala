package tethys.writers.instances

import tethys.{JsonObjectWriter, JsonWriter, specializations}
import tethys.writers.KeyWriter
import tethys.writers.tokens.TokenWriter

private[tethys] trait MapWriters extends IterableWriters {
  implicit def mapWriter[K, A](implicit
      keyWriter: KeyWriter[K],
      valueWriter: JsonWriter[A]
  ): JsonObjectWriter[Map[K, A]] = new JsonObjectWriter[Map[K, A]] {
    override def writeValues(
        value: Map[K, A],
        tokenWriter: TokenWriter
    ): Unit = {
      val valueIterator = value.iterator
      while (valueIterator.hasNext) {
        val v = valueIterator.next()
        tokenWriter.writeFieldName(keyWriter.toKey(v._1))
        valueWriter.write(v._2, tokenWriter)
      }
    }
  }
}
