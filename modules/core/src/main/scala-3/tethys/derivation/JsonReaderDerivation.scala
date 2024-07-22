package tethys.derivation

import tethys.commons.TokenNode
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.readers.{FieldName, ReaderError}
import tethys.JsonReader
import tethys.ReaderBuilder

import scala.collection.mutable
import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonFrom, summonInline}


private [tethys]
trait JsonReaderDerivation:
  def const[A](value: A): JsonReader[A] =
    new JsonReader[A]:
      override def read(it: TokenIterator)(implicit fieldName: FieldName): A =
        if !it.currentToken().isObjectStart then
          ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)
        else {
          it.skipExpression()
          value
        }

  inline def derived[A](inline config: ReaderBuilder[A])(using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    Derivation.deriveJsonReaderForProduct[A](config)


  inline def derived[A](using mirror: Mirror.Of[A]): JsonReader[A] =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        Derivation.deriveJsonReaderForProduct[A](
          summonFrom[ReaderBuilder[A]] {
            case config: ReaderBuilder[A] => config
            case _ => ReaderBuilder[A]
          }
        )
      case given Mirror.SumOf[A] =>
        Derivation.deriveJsonReaderForSum[A]