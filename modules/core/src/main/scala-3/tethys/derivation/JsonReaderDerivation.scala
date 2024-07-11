package tethys.derivation

import tethys.commons.TokenNode
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.readers.{FieldName, ReaderError}
import tethys.{JsonReader, JsonConfig}
import tethys.ReaderBuilder

import scala.collection.mutable
import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonFrom, summonInline}


private [tethys]
trait JsonReaderDerivation:

  inline def derived[A](inline config: ReaderBuilder[A])(using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    Derivation.deriveJsonReaderForProduct(config)
  
  inline def derived[A](inline config: JsonConfig[A])(using mirror: Mirror.SumOf[A]): JsonReader[A] =
    Derivation.deriveJsonReaderForSum(config)


  inline def derived[A](using mirror: Mirror.Of[A]): JsonReader[A] =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        derived(
          summonFrom[ReaderBuilder[A]] {
            case config: ReaderBuilder[A] => config
            case _ => ReaderBuilder[A]
          }
        )
      case given Mirror.SumOf[A] =>
        derived(
          summonFrom[JsonConfig[A]] {
            case config: JsonConfig[A] => config
            case _ => JsonConfig[A]
          }
        )