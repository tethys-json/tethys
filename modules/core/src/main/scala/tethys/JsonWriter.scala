package tethys

import tethys.writers.instances.{AllJsonWriters, SimpleJsonObjectWriter}
import tethys.writers.tokens.TokenWriter

import scala.language.higherKinds

trait JsonWriter[@specialized(specializations) A] {
  self =>

  def write(name: String, value: A, tokenWriter: TokenWriter): Unit = {
    tokenWriter.writeFieldName(name)
    write(value, tokenWriter)
  }

  def write(value: A, tokenWriter: TokenWriter): Unit

  def contramap[B](fun: B => A): JsonWriter[B] = new JsonWriter[B] {
    override def write(name: String, value: B, tokenWriter: TokenWriter): Unit = {
      self.write(name, fun(value), tokenWriter)
    }

    override def write(value: B, tokenWriter: TokenWriter): Unit = {
      self.write(fun(value), tokenWriter)
    }
  }
}

object JsonWriter extends AllJsonWriters with derivation.JsonObjectWriterDerivation {

  def apply[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[A] = jsonWriter

  def obj[A]: SimpleJsonObjectWriter[A] = SimpleJsonObjectWriter[A]
}
