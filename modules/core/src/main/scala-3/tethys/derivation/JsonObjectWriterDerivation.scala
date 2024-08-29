package tethys.derivation

import tethys.derivation.builder.WriterDerivationConfig

import scala.deriving.Mirror
import tethys.{JsonObjectWriter, JsonWriter, WriterBuilder}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{
  constValueTuple,
  erasedValue,
  summonFrom,
  summonInline
}

private[tethys] trait JsonObjectWriterDerivation:

  inline def derived[A](inline config: WriterBuilder[A])(using
      mirror: Mirror.ProductOf[A]
  ) =
    Derivation.deriveJsonWriterForProduct[A](config)

  @deprecated("Use WriterBuilder instead")
  inline def derived[A](inline config: WriterDerivationConfig)(using
      mirror: Mirror.Of[A]
  ) =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        Derivation.deriveJsonWriterForProductLegacy[A](config)

      case given Mirror.SumOf[A] =>
        Derivation.deriveJsonWriterForSumLegacy[A](config)

  inline def derived[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        Derivation.deriveJsonWriterForProduct[A](
          summonFrom[WriterBuilder[A]] {
            case config: WriterBuilder[A] =>
              config
            case _ => WriterBuilder[A]
          }
        )

      case given Mirror.SumOf[A] =>
        Derivation.deriveJsonWriterForSum[A]
