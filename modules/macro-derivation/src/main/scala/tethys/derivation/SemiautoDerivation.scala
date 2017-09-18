package tethys.derivation

import tethys.JsonReader
import tethys.derivation.builder.{WriterBuilder, WriterDescription}
import tethys.derivation.impl.builder.WriterDescriptorMacro
import tethys.derivation.impl.derivation.SemiautoDerivationMacro
import tethys.writers.JsonObjectWriter

import scala.language.experimental.macros

trait SemiautoDerivation {
  def jsonWriter[A]: JsonObjectWriter[A] = macro SemiautoDerivationMacro.simpleJsonWriter[A]
  def jsonWriter[A](description: WriterDescription[A]): JsonObjectWriter[A] = macro SemiautoDerivationMacro.describedJsonWriter[A]

  def describe[A](builder: WriterBuilder[A]): WriterDescription[A] = macro WriterDescriptorMacro.simpleDescription[A]

  def jsonReader[A]: JsonReader[A] = macro SemiautoDerivationMacro.simpleJsonReader[A]
}
