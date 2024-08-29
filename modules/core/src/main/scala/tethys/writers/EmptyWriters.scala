package tethys.writers

import tethys.JsonWriter
import tethys.writers.tokens.TokenWriter

trait EmptyWriters {
  def emptyWriter[A]: JsonWriter[A] = new JsonWriter[A] {
    override def write(name: String, value: A, tokenWriter: TokenWriter): Unit =
      ()
    override def write(value: A, tokenWriter: TokenWriter): Unit = ()
  }
}

object EmptyWriters extends EmptyWriters
