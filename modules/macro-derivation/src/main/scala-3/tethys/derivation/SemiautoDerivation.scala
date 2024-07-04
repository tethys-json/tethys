package tethys.derivation

import tethys.{JsonObjectWriter, JsonReader, ReaderBuilder, WriterBuilder}
import tethys.derivation.builder.{ReaderDerivationConfig, ReaderDescription, WriterDerivationConfig, WriterDescription}

import scala.deriving.Mirror

trait SemiautoDerivation {

  private inline def failWriterDerivationForEnum[T]: Unit =
    inline if EnumCompanion.isEnum[T] then
      scala.compiletime.error(
        """
           Old Enum derivation is not supported anymore

           Use JsonObjectWriter.derived for complex enums like this:
             enum ComplexEnum:
               case A(x: B)
               case B

           Use StringEnumWriter.derived or OrdinalEnumWriter.derived for basic enums like this:
             enum BasicEnum:
               case A, B

           Use StringEnumWriter.withLabel("__type") or OrdinalEnumWriter.withLabel("__type") if you want write an object for BasicEnum like
             { "__type": A }
           """
      )

  private inline def failReaderDerivationForEnum[T]: Unit =
    inline if EnumCompanion.isEnum[T] then
      scala.compiletime.error(
        """
           Old Enum derivation is not supported anymore

           Use StringEnumReader.derived or OrdinalEnumReader.derived for basic enums like this:
             enum BasicEnum:
               case A, B
           """
      )



  @deprecated("Use JsonObjectWriter.derived instead")
  inline def jsonWriter[T](using mirror: Mirror.Of[T]): JsonObjectWriter[T] =
    failWriterDerivationForEnum[T]
    JsonObjectWriter.derived[T]

  inline def jsonWriter[T](inline description: WriterDescription[T]): JsonObjectWriter[T] =
    scala.compiletime.error("Use WriterBuilder[T] directly")

  @deprecated("Use JsonObjectWriter.derived or derives instead")
  inline def jsonWriter[T](inline builder: WriterBuilder[T])(using mirror: Mirror.ProductOf[T]): JsonObjectWriter[T] =
    failWriterDerivationForEnum[T]
    JsonObjectWriter.derived[T](builder)

  inline def jsonWriter[T](inline config: WriterDerivationConfig): JsonObjectWriter[T] =
    scala.compiletime.error("Use WriterBuilder[T] instead")

  @deprecated("Use WriterBuilder[T] directly")
  inline def describe[T <: Product](inline builder: WriterBuilder[T]): WriterBuilder[T] =
    scala.compiletime.error("Use WriterBuilder[T] directly")

  @deprecated("Use JsonReader.derived instead")
  inline def jsonReader[T](using mirror: Mirror.Of[T]): JsonReader[T] =
    failReaderDerivationForEnum[T]
    JsonReader.derived[T]

  inline def jsonReader[T](inline description: ReaderDescription[T]): JsonReader[T] =
    scala.compiletime.error("Use ReaderBuilder[T] instead")

  inline def jsonReader[T](inline config: ReaderDerivationConfig): JsonReader[T] =
    scala.compiletime.error("Use ReaderBuilder[T] instead")

  @deprecated("Use JsonReader.derived and derives instead")
  inline def jsonReader[T](inline builder: ReaderBuilder[T])(using mirror: Mirror.ProductOf[T]): JsonReader[T] =
    failReaderDerivationForEnum[T]
    JsonReader.derived(builder)

  @deprecated("Use ReaderBuilder[T] directly")
  inline def describe[T <: Product](inline builder: ReaderBuilder[T]): ReaderBuilder[T] =
    builder
}
