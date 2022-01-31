package tethys.derivation.impl.builder

import scala.quoted.*

import tethys.derivation.builder.{WriterBuilder, WriterDerivationConfig, WriterDescription}
import tethys.derivation.impl.builder.WriterBuilderUtils

trait WriterBuilderCommons extends WriterBuilderUtils {
  import context.reflect.*

  protected def convertWriterBuilder[T <: Product: Type](
      builder: Expr[WriterBuilder[T]]
  ): Expr[WriterDescription[T]] = {
    val withoutInlining = (builder.asTerm match {
      case Inlined(_, _, expansion) => expansion
      case notInlined => notInlined
    }).asExprOf[WriterBuilder[T]]
    val description = extractSimpleDescription(withoutInlining)
    checkOperations(description.operations)
    description.lift
  }

  protected lazy val emptyWriterConfig: Expr[WriterDerivationConfig] =
    '{ tethys.derivation.builder.WriterDerivationConfig.empty }

  private def extractSimpleDescription[T <: Product: Type](expr: Expr[WriterBuilder[T]]): MacroWriteDescription = {
    expr match {
      case '{
        type tpe <: T
        WriterBuilder.apply[`tpe`]
      } => MacroWriteDescription(TypeRepr.of[tpe], emptyWriterConfig, Seq())

      // ===== remove =====
      case '{ ($rest: WriterBuilder[T]).remove[tpe](${BuilderField(f)}) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.Remove(description.tpe, f.name)
        )

      // ===== rename =====
      case '{ ($rest: WriterBuilder[T]).rename[a](${BuilderField(f)})($rename) } =>
        val description = extractSimpleDescription(rest)
        val aTpr = TypeRepr.of[a]
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.Update(
              description.tpe,
              f.name,
              '{ Some($rename) },
              '{ identity[a] }.asTerm,
              aTpr,
              aTpr
            )
        )

      // ===== update =====
      case '{ ($rest: WriterBuilder[T]).update[a](${BuilderField(f)}).apply[b]($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.Update(
              description.tpe,
              f.name,
              '{ None },
              updater.asTerm,
              TypeRepr.of[a],
              TypeRepr.of[b]
            )
        )

      // ===== update with rename =====
      case '{ ($rest: WriterBuilder[T]).update[a](${BuilderField(f)}).withRename($rename).apply[b]($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.Update(
              description.tpe,
              f.name,
              '{ Some($rename) },
              updater.asTerm,
              TypeRepr.of[a],
              TypeRepr.of[b]
            )
        )

      // ===== update from root =====
      case '{ ($rest: WriterBuilder[T]).update(${BuilderField(f)}).fromRoot[b]($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.UpdateFromRoot(
              description.tpe,
              f.name,
              '{ None },
              updater.asTerm,
              TypeRepr.of[b]
            )
        )

      // ===== update from root with rename =====
      case '{ ($rest: WriterBuilder[T]).update(${BuilderField(f)}).withRename($rename).fromRoot[b]($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.UpdateFromRoot(
              description.tpe,
              f.name,
              '{ Some($rename) },
              updater.asTerm,
              TypeRepr.of[b]
            )
        )

      // ===== add =====
      case '{ ($rest: WriterBuilder[T]).add($name).apply[a]($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.Add(description.tpe, name, updater.asTerm, TypeRepr.of[a])
        )

      // ===== update partial =====
      case '{ ($rest: WriterBuilder[T]).updatePartial[a](${BuilderField(f)}).apply($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.UpdatePartial(
              description.tpe,
              f.name,
              '{ None },
              updater.asTerm,
              TypeRepr.of[a]
            )
        )

      // ===== update partial with rename =====
      case '{ ($rest: WriterBuilder[T]).updatePartial[a](${BuilderField(f)}).withRename($rename).apply($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.UpdatePartial(
              description.tpe,
              f.name,
              '{ Some($rename) },
              updater.asTerm,
              TypeRepr.of[a]
            )
        )

      // ===== update partial from root =====
      case '{ ($rest: WriterBuilder[T]).updatePartial[a](${BuilderField(f)}).fromRoot($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.UpdatePartialFromRoot(description.tpe, f.name, '{ None }, updater.asTerm)
        )

      // ===== update partial from root with rename =====
      case '{ ($rest: WriterBuilder[T]).updatePartial[a](${BuilderField(f)}).withRename($rename).fromRoot($updater) } =>
        val description = extractSimpleDescription(rest)
        description.copy(operations =
          description.operations :+
            WriterMacroOperation.UpdatePartialFromRoot(
              description.tpe,
              f.name,
              '{ Some($rename) },
              updater.asTerm
            )
        )

      // ===== FieldStyle =====
      case '{ ($rest: WriterBuilder[T]).fieldStyle($style) } =>
        val description = extractSimpleDescription(rest)
        description.copy(config = '{ (${ description.config }: WriterDerivationConfig).withFieldStyle($style) })

      // ===== NOPE =====
      case _ => report.errorAndAbort(s"unknown tree: ${expr.asTerm.show}")
    }
  }

  private def checkOperations(operations: Seq[WriterMacroOperation]): Unit = {
    // TODO
  }
}
