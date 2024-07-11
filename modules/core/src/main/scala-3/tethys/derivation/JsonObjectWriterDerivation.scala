package tethys.derivation

import scala.deriving.Mirror
import tethys.{JsonConfig, JsonObjectWriter, JsonWriter, WriterBuilder}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, summonFrom, summonInline}

private[tethys] trait JsonObjectWriterDerivation:

  inline def derived[A](inline config: WriterBuilder[A])(using mirror: Mirror.ProductOf[A]) =
    Derivation.deriveJsonWriterForProduct[A](config)

  inline def derived[A](inline config: JsonConfig[A])(using m: Mirror.SumOf[A]) =
    Derivation.deriveJsonWriterForSum[A](config)

  inline def derived[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        derived(
          summonFrom[WriterBuilder[A]] {
            case config: WriterBuilder[A] =>
              config
            case _ => WriterBuilder[A]
          }
        )

      case given Mirror.SumOf[A] =>
        derived(
          summonFrom[JsonConfig[A]] {
            case config: JsonConfig[A] =>
              config
            case _ => 
              JsonConfig[A]
          }
        )
        