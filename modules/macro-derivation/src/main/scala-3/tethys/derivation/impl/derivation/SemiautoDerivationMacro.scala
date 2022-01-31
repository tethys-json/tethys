package tethys.derivation.impl.derivation

import scala.quoted.*

import tethys.derivation.builder.*
import tethys.derivation.impl.builder.{WriterBuilderCommons, WriterBuilderUtils}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonObjectWriter, JsonReader}
import tethys.writers.tokens.TokenWriter

class SemiautoDerivationMacro(val quotes: Quotes) extends WriterDerivation with ReaderDerivation {
  implicit val context: Quotes = quotes
  import context.reflect.*

  def simpleJsonWriter[T: Type]: Expr[JsonObjectWriter[T]] = {
    val tpe = TypeRepr.of[T]
    val description = MacroWriteDescription(
      tpe = tpe,
      config = emptyWriterConfig,
      operations = Seq()
    )
    jsonWriterWithMacroWriteDescription[T](description)
  }

  def jsonWriterWithConfig[T: Type](config: Expr[WriterDerivationConfig]): Expr[JsonObjectWriter[T]] = {
    val tpe = TypeRepr.of[T]

    val description = MacroWriteDescription(
      tpe = tpe,
      config = config,
      operations = Seq()
    )

    jsonWriterWithMacroWriteDescription[T](description)
  }

  def jsonWriterWithMacroWriteDescription[T: Type](description: MacroWriteDescription): Expr[JsonObjectWriter[T]] = {
    val tpeSym = TypeRepr.of[T].typeSymbol

    '{
      new JsonObjectWriter[T] {
        def writeValues(value: T, tokenWriter: TokenWriter): Unit = ${
          if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
            deriveCaseClassWriteValuesWithDescription[T]('value, 'tokenWriter)(description)
          else if (
            tpeSym.flags.is(Flags.Enum) || (tpeSym.flags
              .is(Flags.Sealed) && (tpeSym.flags.is(Flags.Trait) || tpeSym.flags.is(Flags.Abstract)))
          )
            deriveSealedClassWriteValues[T]('value, 'tokenWriter)(description.config)
          else
            report.errorAndAbort(s"Can't auto derive json writer! '${tpeSym.fullName}' isn't a Case Class, Sealed Trait, Sealed Abstract Class or Enum.")
        }
      }
    }
  }

  def jsonWriterWithBuilder[T <: Product : Type](builder: Expr[WriterBuilder[T]]): Expr[JsonObjectWriter[T]] = {
    val description = convertWriterBuilder[T](builder)
    jsonWriterWithWriterDescription[T](description)
  }

  def jsonWriterWithWriterDescription[T: Type](description: Expr[WriterDescription[T]]): Expr[JsonObjectWriter[T]] = {
    val tpeSym = TypeRepr.of[T].typeSymbol
    if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
      '{
        new JsonObjectWriter[T] {
          override def writeValues(value: T, tokenWriter: TokenWriter): Unit = ${
            deriveCaseClassWriteValuesWithDescription[T]('value, 'tokenWriter)(
              MacroWriteDescription.unlift(description)
            )
          }
        }
      }
    else report.errorAndAbort(s"Can't derive json writer! ${tpeSym.fullName} isn't a Case Class.")
  }

  def simpleJsonReader[T: Type]: Expr[JsonReader[T]] = {
    val description = MacroReaderDescription(
      config = emptyReaderConfig,
      operations = Seq()
    )
    jsonReaderWithMacroReaderDescription[T](description)
  }

  def jsonReaderWithConfig[T: Type](config: Expr[ReaderDerivationConfig]): Expr[JsonReader[T]] = {
    val description = MacroReaderDescription(
      config = config,
      operations = Seq()
    )

    jsonReaderWithMacroReaderDescription[T](description)
  }

  def jsonReaderWithMacroReaderDescription[T: Type](description: MacroReaderDescription): Expr[JsonReader[T]] = {
    val tpeSym = TypeRepr.of[T].typeSymbol
    if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
      deriveCaseClassReadWithDescription[T](description)
    else
      report.errorAndAbort(s"Can't derive json reader! '${tpeSym.fullName}' isn't a Case Class.")
  }

  def jsonReaderWithBuilder[T <: Product : Type](builder: Expr[ReaderBuilder[T]]): Expr[JsonReader[T]] = {
    val description = convertReaderBuilder[T](builder)
    jsonReaderWithReaderDescription[T](description)
  }


  def jsonReaderWithReaderDescription[T: Type](description: Expr[ReaderDescription[T]]): Expr[JsonReader[T]] = {
    val tpeSym = TypeRepr.of[T].typeSymbol
    if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
      deriveCaseClassReadWithDescription[T](MacroReaderDescription.unlift(description))
    else
      report.errorAndAbort(s"Can't derive json reader! '${tpeSym.fullName}' isn't a Case Class.")
  }
}
