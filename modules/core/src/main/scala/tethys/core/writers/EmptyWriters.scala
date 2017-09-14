package tethys.core.writers

import tethys.core.writers.token.TokenWriter

trait EmptyWriters {
  def emptyWriter[A]: JsonWriter[A] = new JsonWriter[A] {
    override def write(name: String, value: A, tokenWriter: TokenWriter): Unit = ()
    override def write(value: A, tokenWriter: TokenWriter): Unit = ()
  }
}

object EmptyWriters extends EmptyWriters
