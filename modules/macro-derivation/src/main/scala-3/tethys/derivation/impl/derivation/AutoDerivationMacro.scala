package tethys.derivation.impl.derivation

import scala.quoted.*

import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.commons.LowPriorityInstance
import tethys.writers.tokens.TokenWriter

class AutoDerivationMacro(val quotes: Quotes) extends WriterDerivation with ReaderDerivation {
  implicit val context: Quotes = quotes
  import context.reflect.*

  def simpleJsonWriter[T: Type]: Expr[LowPriorityInstance[JsonObjectWriter[T]]] = {
    // TODO: recursive derivation check

    val tpe = TypeRepr.of[T]
    val description = MacroWriteDescription(
      tpe = tpe,
      config = emptyWriterConfig,
      operations = Seq()
    )
    val tpeSym = tpe.typeSymbol

    '{
      LowPriorityInstance[JsonObjectWriter[T]](
        new JsonObjectWriter[T] {
          private[this] implicit def thisWriter: JsonWriter[T] = this

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
      )
    }
  }

  def simpleJsonReader[T: Type]: Expr[LowPriorityInstance[JsonReader[T]]] = {
    val description = MacroReaderDescription(
      config = emptyReaderConfig,
      operations = Seq()
    )
    val tpeSym = TypeRepr.of[T].typeSymbol
    if (tpeSym.isClassDef && tpeSym.flags.is(Flags.Case))
      '{ LowPriorityInstance[JsonReader[T]](${deriveCaseClassReadWithDescription[T](description)}) }
    else
      report.errorAndAbort(s"Can't auto derive json reader! '${tpeSym.fullName}' isn't a Case Class")
  }
}
