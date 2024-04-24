package tethys.derivation

import tethys.{JsonConfig, JsonObjectWriter, JsonWriter}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{constValueTuple, erasedValue, summonFrom, summonInline}

private[tethys] trait JsonObjectWriterDerivation extends JsonWriterConfiguration:

  inline def derived[A](inline config: JsonWriter.ProductConfig[A])(using mirror: Mirror.ProductOf[A]) =
    new JsonObjectWriter[A]:
      lazy val configuration: JsonWriterProductConfigParsed[A] = Derivation.parseJsonWriterProductConfig[A](config)

      override def writeValues(value: A, tokenWriter: TokenWriter): Unit =
        configuration.fields.foreach { field =>
          field.writer.write(field.label, field.function(value), tokenWriter)
        }

  inline def derived[A](inline config: JsonConfig[A])(using m: Mirror.SumOf[A]) =
    new JsonObjectWriter[A]:
      lazy val writers = summonJsonWritersForSumArray[A, m.MirroredElemTypes]

      override def writeValues(value: A, tokenWriter: TokenWriter): Unit =
        Derivation.writeDiscriminator[A](config)(value, tokenWriter)
        writers(m.ordinal(value)).writeValues(value.asInstanceOf, tokenWriter)


  inline def derived[A](using mirror: Mirror.Of[A]): JsonObjectWriter[A] =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        derived(
          summonFrom[JsonWriter.ProductConfig[A]] {
            case config: JsonWriter.ProductConfig[A] =>
              config
            case _ => JsonWriter.configure[A]
          }
        )

      case given Mirror.SumOf[A] =>
        derived(
          summonFrom[JsonConfig[A]] {
            case config: JsonConfig[A] =>
              config
            case _ => 
              JsonConfig.configure[A]
          }
        )

  private inline def summonJsonWritersForSumArray[T, Elems <: Tuple]: Array[JsonObjectWriter[?]] =
    summonJsonWritersForSum[T, Elems].toArray

  private inline def summonJsonWritersForSum[T, Elems <: Tuple]: List[JsonObjectWriter[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Nil
      case _: (elem *: elems) =>
        summonOrDeriveJsonWriterForSum[T, elem] :: summonJsonWritersForSum[T, elems]

  private inline def summonOrDeriveJsonWriterForSum[T, Elem]: JsonObjectWriter[Elem] =
    summonFrom[JsonWriter[Elem]] {
      case writer: JsonObjectWriter[Elem] =>
        writer
      case writer: JsonWriter[Elem] =>
        scala.compiletime.error("JsonObjectWriter required for the children types, but JsonWriter found")
      case _ =>
        JsonObjectWriterDerivation.deriveRec[T, Elem]
    }

private[tethys]
object JsonObjectWriterDerivation:
  inline def summonOrDeriveJsonWriterForField[T, Field]: JsonWriter[Field] =
    summonFrom[Mirror.Of[Field]] {
      case mirror: Mirror.Of[Field] =>
        summonFrom[JsonWriter[Field]] {
          case writer: JsonWriter[Field] =>
            writer
          case _ =>
            deriveRec[T, Field]
        }
      case _ =>
        summonInline[JsonWriter[Field]]
    }


  inline def deriveRec[T, Elem]: JsonObjectWriter[Elem] =
    inline erasedValue[T] match
      case _: Elem =>
        scala.compiletime.error("Recursive derivation is not possible")
      case value =>
        JsonWriter.derived[Elem](using summonInline[Mirror.Of[Elem]])
