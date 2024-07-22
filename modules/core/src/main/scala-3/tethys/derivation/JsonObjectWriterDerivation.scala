package tethys.derivation

import scala.deriving.Mirror
import tethys.{JsonObjectWriter, JsonWriter, WriterBuilder}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, summonFrom, summonInline}

private[tethys] trait JsonObjectWriterDerivation:

  inline def derived[A](inline config: WriterBuilder[A])(using mirror: Mirror.ProductOf[A]) =
    Derivation.deriveJsonWriterForProduct[A](config)

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
        