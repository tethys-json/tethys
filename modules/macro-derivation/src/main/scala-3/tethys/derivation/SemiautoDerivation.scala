package tethys.derivation

import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.derivation.builder.{
  NotDescribedException,
  ReaderBuilder,
  ReaderDerivationConfig,
  ReaderDescription,
  ReaderField,
  WriterBuilder,
  WriterDerivationConfig,
  WriterDescription
}
import tethys.derivation.impl.builder.{ReaderDescriptionMacro, WriterDescriptionMacro}
import tethys.derivation.impl.derivation.SemiautoDerivationMacro
import scala.quoted.*
import scala.annotation.compileTimeOnly
import scala.annotation.experimental

trait SemiautoDerivation {
  @deprecated("Use JsonObjectWriter.derived or JsonWriter.derived instead")
  inline def jsonWriter[T]: JsonObjectWriter[T] =
    JsonWriter.derived[T](using scala.compiletime.summonInline[scala.deriving.Mirror.Of[T]])

  inline def jsonWriter[T](inline description: WriterDescription[T]): JsonObjectWriter[T] =
    ${ SemiautoDerivation.jsonWriterWithDescription[T]('description) }

  inline def jsonWriter[T <: Product](inline builder: => WriterBuilder[T]): JsonObjectWriter[T] =
    ${ SemiautoDerivation.jsonWriterWithBuilder[T]('builder) }

  inline def jsonWriter[T](inline config: WriterDerivationConfig): JsonObjectWriter[T] =
    ${ SemiautoDerivation.jsonWriterWithConfig[T]('config) }

  inline def describe[T <: Product](inline builder: => WriterBuilder[T]): WriterDescription[T] =
    ${ SemiautoDerivation.describeWriter[T]('builder) }

  @deprecated("Use JsonReader.derived instead")
  inline def jsonReader[T]: JsonReader[T] =
    JsonReader.derived[T](using scala.compiletime.summonInline[scala.deriving.Mirror.ProductOf[T]])

  inline def jsonReader[T](inline description: ReaderDescription[T]): JsonReader[T] =
    ${ SemiautoDerivation.jsonReaderWithDescription[T]('description) }

  inline def jsonReader[T](inline config: ReaderDerivationConfig): JsonReader[T] =
    ${ SemiautoDerivation.jsonReaderWithConfig[T]('config) }

  inline def jsonReader[T <: Product](inline builder: => ReaderBuilder[T]): JsonReader[T] =
    ${ SemiautoDerivation.jsonReaderWithBuilder[T]('builder) }

  inline def describe[T <: Product](inline builder: => ReaderBuilder[T]): ReaderDescription[T] =
    ${ SemiautoDerivation.describeReader[T]('builder) }

  implicit class ReaderFieldStringOps(val s: String) {
    @compileTimeOnly("ReaderFieldOps.as should be defined in describe block")
    def as[A]: ReaderField[A] = throw new NotDescribedException
  }

  implicit class ReaderFieldSymbolOps(val s: Symbol) {
    @compileTimeOnly("ReaderFieldOps.as should be defined in describe block")
    def as[A]: ReaderField[A] = throw new NotDescribedException
  }
}

private[this] object SemiautoDerivation {
  @experimental
  def jsonWriter[T: Type](using Quotes): Expr[JsonObjectWriter[T]] =
    new SemiautoDerivationMacro(quotes).simpleJsonWriter[T]

  @experimental
  def jsonWriterWithConfig[T: Type](config: Expr[WriterDerivationConfig])(using Quotes): Expr[JsonObjectWriter[T]] =
    new SemiautoDerivationMacro(quotes).jsonWriterWithConfig[T](config)

  @experimental
  def jsonWriterWithDescription[T: Type](description: Expr[WriterDescription[T]])(using
      Quotes
  ): Expr[JsonObjectWriter[T]] =
    new SemiautoDerivationMacro(quotes).jsonWriterWithWriterDescription[T](description)

  @experimental
  def jsonWriterWithBuilder[T <: Product: Type](builder: Expr[WriterBuilder[T]])(using
      Quotes
  ): Expr[JsonObjectWriter[T]] =
    new SemiautoDerivationMacro(quotes).jsonWriterWithBuilder[T](builder)

  @experimental
  def describeWriter[T <: Product: Type](builder: Expr[WriterBuilder[T]])(using Quotes): Expr[WriterDescription[T]] =
    new WriterDescriptionMacro(quotes).simpleDescription[T](builder)

  @experimental
  def jsonReader[T: Type](using Quotes): Expr[JsonReader[T]] =
    new SemiautoDerivationMacro(quotes).simpleJsonReader[T]

  def jsonReaderWithConfig[T: Type](config: Expr[ReaderDerivationConfig])(using Quotes): Expr[JsonReader[T]] =
    new SemiautoDerivationMacro(quotes).jsonReaderWithConfig[T](config)

  def jsonReaderWithDescription[T: Type](description: Expr[ReaderDescription[T]])(using Quotes): Expr[JsonReader[T]] =
    new SemiautoDerivationMacro(quotes).jsonReaderWithReaderDescription[T](description)

  def jsonReaderWithBuilder[T <: Product: Type](builder: Expr[ReaderBuilder[T]])(using Quotes): Expr[JsonReader[T]] =
    new SemiautoDerivationMacro(quotes).jsonReaderWithBuilder[T](builder)

  def describeReader[T <: Product: Type](builder: Expr[ReaderBuilder[T]])(using Quotes): Expr[ReaderDescription[T]] =
    new ReaderDescriptionMacro(quotes).simpleDescription[T](builder)
}
