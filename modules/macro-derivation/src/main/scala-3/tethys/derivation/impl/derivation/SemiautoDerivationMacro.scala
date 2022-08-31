package tethys.derivation.impl.derivation

import scala.quoted.*

import tethys.derivation.builder.*
import tethys.derivation.impl.builder.{WriterBuilderCommons, WriterBuilderUtils}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
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
    val tpe = TypeRepr.of[T]

    if (tpe.termSymbol.isNoSymbol) {
      val tpeSym = tpe.typeSymbol

      if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
        deriveCaseClassWriter[T](description)
      else if (tpeSym.flags.is(Flags.Enum) || (tpeSym.flags.is(Flags.Sealed) && (tpeSym.flags.is(Flags.Trait) || tpeSym.flags.is(Flags.Abstract))))
        deriveSealedClassWriter[T](description.config)
      else
        report.errorAndAbort(
          s"Can't auto derive json writer! ${tpe.show} isn't a Case Class, Sealed Trait, Sealed Abstract Class or Enum"
        )
    }
    else deriveTermWriter[T]
  }

  def jsonWriterWithBuilder[T <: Product: Type](builder: Expr[WriterBuilder[T]]): Expr[JsonObjectWriter[T]] = {
    val description = convertWriterBuilder[T](builder)
    jsonWriterWithWriterDescription[T](description)
  }

  def jsonWriterWithWriterDescription[T: Type](description: Expr[WriterDescription[T]]): Expr[JsonObjectWriter[T]] = {
    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol
    if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
      deriveCaseClassWriter[T](MacroWriteDescription.unlift(description))
    else report.errorAndAbort(s"Can't derive json writer! ${tpe.show} isn't a Case Class")
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
    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol

    if (tpe.termSymbol.isNoSymbol) {
      if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
        deriveCaseClassReader[T](description)
      else if (tpeSym.flags.is(Flags.Enum | Flags.Abstract))
        deriveEnumReader[T]
      else
        report.errorAndAbort(s"Can't derive json reader! '${tpe.show}' isn't a Case Class")
    }
    else deriveTermReader[T]
  }

  def jsonReaderWithBuilder[T <: Product: Type](builder: Expr[ReaderBuilder[T]]): Expr[JsonReader[T]] = {
    val description = convertReaderBuilder[T](builder)
    jsonReaderWithReaderDescription[T](description)
  }

  def jsonReaderWithReaderDescription[T: Type](description: Expr[ReaderDescription[T]]): Expr[JsonReader[T]] = {
    val tpe = TypeRepr.of[T]
    val tpeSym = tpe.typeSymbol
    if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
      deriveCaseClassReader[T](MacroReaderDescription.unlift(description))
    else
      report.errorAndAbort(s"Can't derive json reader! '${tpe.show}' isn't a Case Class")
  }
}
