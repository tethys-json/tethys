package tethys.derivation

import tethys.core.writers.JsonWriter
import tethys.core.writers.builder.{WriterBuilder, WriterDescription}
import tethys.derivation.impl.builder.WriterDescriptorMacro
import tethys.derivation.impl.derivation.SemiautoDerivationMacro

import scala.language.experimental.macros

trait SemiautoDerivation {
  def jsonWriter[A]: JsonWriter[A] = macro SemiautoDerivationMacro.simpleJsonWriter[A]
  def jsonWriter[A](description: WriterDescription[A]): JsonWriter[A] = macro SemiautoDerivationMacro.describedJsonWriter[A]

  def describe[A](builder: WriterBuilder[A]): WriterDescription[A] = macro WriterDescriptorMacro.simpleDescription[A]
}
