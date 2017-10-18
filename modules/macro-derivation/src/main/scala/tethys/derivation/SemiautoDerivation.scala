package tethys.derivation

import tethys.derivation.builder.{ReaderBuilder, ReaderDescription, WriterBuilder, WriterDescription}
import tethys.derivation.impl.builder.{ReaderDescriptionMacro, WriterDescriptorMacro}
import tethys.derivation.impl.derivation.SemiautoDerivationMacro
import tethys.{JsonObjectWriter, JsonReader}

import scala.language.experimental.macros

trait SemiautoDerivation {
  def jsonWriter[A]: JsonObjectWriter[A] = macro SemiautoDerivationMacro.simpleJsonWriter[A]
  def jsonWriter[A](description: WriterDescription[A]): JsonObjectWriter[A] = macro SemiautoDerivationMacro.describedJsonWriter[A]

  def describe[A](builder: WriterBuilder[A]): WriterDescription[A] = macro WriterDescriptorMacro.simpleDescription[A]

  def jsonReader[A]: JsonReader[A] = macro SemiautoDerivationMacro.simpleJsonReader[A]

  def describe[A](builder: ReaderBuilder[A]): ReaderDescription[A] = macro ReaderDescriptionMacro.readerDescription[A]
}
