package tethys.derivation

import tethys.derivation.builder.WriterDerivationConfig
import tethys.{JsonConfiguration, JsonObjectWriter, WriterBuilder}

import scala.compiletime.summonFrom
import scala.deriving.Mirror

private[tethys] trait JsonObjectWriterDerivation:

  inline def derived[A](inline config: WriterBuilder[A])(using
      mirror: Mirror.ProductOf[A]
  ) =
    Derivation.deriveJsonWriterForProduct[A](config, JsonConfiguration.default)

  inline def derivedWith[A](inline config: WriterBuilder[A])(using
      mirror: Mirror.ProductOf[A]
  ): JsonObjectWriter[A]^ =
    Derivation.deriveJsonWriterForProductWith[A](
      config,
      JsonConfiguration.default
    )

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
          },
          summonFrom[JsonConfiguration] {
            case jsonConfig: JsonConfiguration =>
              jsonConfig
            case _ => JsonConfiguration.default

          }
        )

      case given Mirror.SumOf[A] =>
        Derivation.deriveJsonWriterForSum[A]

  inline def derivedWith[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A]^ =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        Derivation.deriveJsonWriterForProductWith[A](
          summonFrom[WriterBuilder[A]] {
            case config: WriterBuilder[A] =>
              config
            case _ => WriterBuilder[A]
          },
          summonFrom[JsonConfiguration] {
            case jsonConfig: JsonConfiguration =>
              jsonConfig
            case _ => JsonConfiguration.default

          }
        )

      case given Mirror.SumOf[A] =>
        Derivation.deriveJsonWriterForSumWith[A]
