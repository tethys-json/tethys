package tethys.derivation

import tethys.{JsonObjectWriter, JsonWriter}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{summonInline, erasedValue}

private[tethys] trait JsonWriterDerivation:
  inline def derived[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    (value: A, tokenWriter: TokenWriter) =>
      inline mirror match
        case m: Mirror.ProductOf[A] =>
          val product = summonInline[A <:< Product](value)
          product.productElementNames
            .zip(product.productIterator)
            .zip(summonAll[m.MirroredElemTypes])
            .foreach { case ((name, value), writer) =>
              writer.write(name, value.asInstanceOf, tokenWriter)
            }

        case m: Mirror.SumOf[A] =>
          summonAll[m.MirroredElemTypes](m.ordinal(value))
            .asInstanceOf[JsonObjectWriter[A]]
            .writeValues(value, tokenWriter)


  private inline def summonAll[T <: Tuple]: List[JsonWriter[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JsonWriter[t]] :: summonAll[ts]
