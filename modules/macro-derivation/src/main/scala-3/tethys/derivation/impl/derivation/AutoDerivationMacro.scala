package tethys.derivation.impl.derivation

import scala.quoted.*

import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.commons.LowPriorityInstance
import tethys.writers.tokens.TokenWriter

class AutoDerivationMacro(val quotes: Quotes)
    extends WriterDerivation
    with ReaderDerivation {
  implicit val context: Quotes = quotes
  import context.reflect.*

  // TODO: recursive A => B => A derivation check
  def simpleJsonWriter[T: Type]
      : Expr[LowPriorityInstance[JsonObjectWriter[T]]] = {
    val tpe: TypeRepr = TypeRepr.of[T]
    val tpeSym: Symbol = tpe.typeSymbol
    val description: MacroWriteDescription = MacroWriteDescription.empty[T]
    val jsonObjectWriterExpr: Expr[JsonObjectWriter[T]] =
      if (tpe.termSymbol.isNoSymbol) {
        if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
          deriveCaseClassWriter[T](description)
        else if (
          tpeSym.flags.is(Flags.Enum) ||
          (tpeSym.flags.is(Flags.Sealed) && (tpeSym.flags.is(
            Flags.Trait
          ) || tpeSym.flags.is(Flags.Abstract)))
        )
          deriveSealedClassWriter[T](description.config)
        else
          report.errorAndAbort(
            s"Can't auto derive json writer! '${tpe.show}' isn't a Case Class, Sealed Trait, Sealed Abstract Class or Enum."
          )
      } else deriveTermWriter[T]

    '{ LowPriorityInstance[JsonObjectWriter[T]]($jsonObjectWriterExpr) }
  }

  def simpleJsonReader[T: Type]: Expr[LowPriorityInstance[JsonReader[T]]] = {
    val tpe: TypeRepr = TypeRepr.of[T]
    val tpeSym: Symbol = tpe.typeSymbol
    val description: MacroReaderDescription = MacroReaderDescription(
      config = emptyReaderConfig,
      operations = Seq()
    )
    val jsonReaderExpr: Expr[JsonReader[T]] =
      if (tpe.termSymbol.isNoSymbol) {
        if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
          deriveCaseClassReader[T](description)
        else if (tpeSym.flags.is(Flags.Enum))
          deriveEnumReader[T]
        else
          report.errorAndAbort(
            s"Can't auto derive json reader! '${tpe.show}' isn't a Case Class or Enum"
          )
      } else deriveTermReader[T]

    '{ LowPriorityInstance[JsonReader[T]]($jsonReaderExpr) }
  }
}
