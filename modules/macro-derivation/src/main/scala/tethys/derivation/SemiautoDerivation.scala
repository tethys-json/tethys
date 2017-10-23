package tethys.derivation

import tethys.derivation.builder._
import tethys.derivation.impl.builder.{ReaderDescriptionMacro, WriterDescriptorMacro}
import tethys.derivation.impl.derivation.SemiautoDerivationMacro
import tethys.{JsonObjectWriter, JsonReader}

import scala.annotation.compileTimeOnly
import scala.language.experimental.macros

trait SemiautoDerivation {
  def jsonWriter[A]: JsonObjectWriter[A] = macro SemiautoDerivationMacro.simpleJsonWriter[A]
  def jsonWriter[A](description: WriterDescription[A]): JsonObjectWriter[A] = macro SemiautoDerivationMacro.describedJsonWriter[A]

  def describe[A](builder: WriterBuilder[A]): WriterDescription[A] = macro WriterDescriptorMacro.simpleDescription[A]

  def jsonReader[A]: JsonReader[A] = macro SemiautoDerivationMacro.simpleJsonReader[A]
  def jsonReader[A](description: ReaderDescription[A]): JsonReader[A] = macro SemiautoDerivationMacro.describedJsonReader[A]

  def describe[A](builder: ReaderBuilder[A]): ReaderDescription[A] = macro ReaderDescriptionMacro.readerDescription[A]

  implicit class ReaderFieldStringOps(val s: String) {

    @compileTimeOnly("ReaderFieldOps.as should be defined in describe block")
    def as[A]: ReaderField[A] = throw new NotDescribedException
  }

  implicit class ReaderFieldSymbolOps(val s: Symbol) {

    @compileTimeOnly("ReaderFieldOps.as should be defined in describe block")
    def as[A]: ReaderField[A] = throw new NotDescribedException
  }
}
