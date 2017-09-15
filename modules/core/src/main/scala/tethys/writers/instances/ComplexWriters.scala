package tethys.writers.instances

import tethys.JsonWriter
import tethys.writers.KeyWriter
import tethys.writers.tokens.TokenWriter

import scala.collection.GenTraversableOnce
import scala.language.higherKinds

/**
  * Created by eld0727 on 21.04.17.
  */
trait ComplexWriters extends LowPriorityComplexWriters{

  implicit def mapWriter[K, A](implicit keyWriter: KeyWriter[K], valueWriter: JsonWriter[A]): JsonWriter[Map[K, A]] = new JsonWriter[Map[K, A]] {
    override def write(value: Map[K, A], tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeObjectStart()

      val valueIterator = value.iterator
      while(valueIterator.hasNext) {
        val v = valueIterator.next()
        tokenWriter.writeFieldName(keyWriter.toKey(v._1))
        valueWriter.write(v._2, tokenWriter)
      }

      tokenWriter.writeObjectEnd()
    }
  }

  implicit def optionalWriter[A](implicit valueWriter: JsonWriter[A]): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {

    override def write(name: String, value: Option[A], tokenWriter: TokenWriter): Unit = {
      if(value.nonEmpty) {
        valueWriter.write(name, value.get, tokenWriter)
      }
    }

    override def write(value: Option[A], tokenWriter: TokenWriter): Unit = {
      if(value.isEmpty) tokenWriter.writeNull()
      else valueWriter.write(value.get, tokenWriter)
    }
  }
}

private[writers] trait LowPriorityComplexWriters {
  implicit def genTraversableOnceWriter[A, C[X] <: GenTraversableOnce[X]](implicit valueWriter: JsonWriter[A]): JsonWriter[C[A]] = new JsonWriter[C[A]]{
    override def write(value: C[A], tokenWriter: TokenWriter): Unit = {
      tokenWriter.writeArrayStart()

      val valueIterator = value.toIterator
      while(valueIterator.hasNext) {
        val v = valueIterator.next()
        valueWriter.write(v, tokenWriter)
      }

      tokenWriter.writeArrayEnd()
    }
  }
}

