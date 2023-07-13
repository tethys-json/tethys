package tethys.derivation.impl.builder

import scala.annotation.tailrec
import scala.quoted.*

import tethys.derivation.builder.{
  WriterBuilder,
  WriterDerivationConfig,
  WriterDescription
}
import tethys.derivation.impl.builder.WriterBuilderUtils

trait WriterBuilderCommons extends WriterBuilderUtils {
  import context.reflect.*

  protected def convertWriterBuilder[T <: Product: Type](
      builder: Expr[WriterBuilder[T]]
  ): Expr[WriterDescription[T]] = {
    val withoutInlining = (builder.asTerm match {
      case Inlined(_, _, expansion) => expansion
      case notInlined               => notInlined
    }).asExprOf[WriterBuilder[T]]
    val description = extractSimpleDescription(withoutInlining)
    checkOperations(description.operations)
    description.lift
  }

  private def extractSimpleDescription[T <: Product: Type](
      expr: Expr[WriterBuilder[T]]
  ): MacroWriteDescription = {
    @tailrec
    def loop(
        expr: Expr[WriterBuilder[T]],
        config: Expr[WriterDerivationConfig],
        operations: Seq[WriterMacroOperation]
    ): (Expr[WriterDerivationConfig], Seq[WriterMacroOperation]) = {
      expr match {
        case '{
              type tpe <: T;
              WriterBuilder.apply[`tpe`]
            } =>
          config -> operations

        // ===== remove =====
        case '{ ($rest: WriterBuilder[T]).remove(${ BuilderField(f) }) } =>
          val op: WriterMacroOperation =
            WriterMacroOperation.Remove(tpe = TypeRepr.of[T], field = f.name)
          loop(rest, config, op +: operations)

        // ===== rename =====
        case '{
              ($rest: WriterBuilder[T])
                .rename[from](${ BuilderField(f) })($rename)
            } =>
          val fromTpe = TypeRepr.of[from]
          val op: WriterMacroOperation = WriterMacroOperation.Update(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ Some($rename) },
            fun = '{ identity[from] }.asTerm,
            from = fromTpe,
            to = fromTpe
          )
          loop(rest, config, op +: operations)

        // ===== update =====
        case '{
              ($rest: WriterBuilder[T])
                .update[from](${ BuilderField(f) })
                .apply[to]($updater)
            } =>
          val op: WriterMacroOperation = WriterMacroOperation.Update(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ None },
            fun = updater.asTerm,
            from = TypeRepr.of[from],
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update with rename =====
        case '{
              ($rest: WriterBuilder[T])
                .update[from](${ BuilderField(f) })
                .withRename($rename)
                .apply[to]($updater)
            } =>
          val op: WriterMacroOperation = WriterMacroOperation.Update(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ Some($rename) },
            fun = updater.asTerm,
            from = TypeRepr.of[from],
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update from root =====
        case '{
              ($rest: WriterBuilder[T])
                .update(${ BuilderField(f) })
                .fromRoot[to]($updater)
            } =>
          val op: WriterMacroOperation = WriterMacroOperation.UpdateFromRoot(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ None },
            fun = updater.asTerm,
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update from root with rename =====
        case '{
              ($rest: WriterBuilder[T])
                .update(${ BuilderField(f) })
                .withRename($rename)
                .fromRoot[to]($updater)
            } =>
          val op: WriterMacroOperation = WriterMacroOperation.UpdateFromRoot(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ Some($rename) },
            fun = updater.asTerm,
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update partial =====
        case '{
              ($rest: WriterBuilder[T])
                .updatePartial[from](${ BuilderField(f) })
                .apply[to]($updater)
            } =>
          val op: WriterMacroOperation = WriterMacroOperation.UpdatePartial(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ None },
            fun = updater.asTerm,
            from = TypeRepr.of[from],
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update partial with rename =====
        case '{
              ($rest: WriterBuilder[T])
                .updatePartial[from](${ BuilderField(f) })
                .withRename($rename)
                .apply[to]($updater)
            } =>
          val op: WriterMacroOperation = WriterMacroOperation.UpdatePartial(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ Some($rename) },
            fun = updater.asTerm,
            from = TypeRepr.of[from],
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update partial from root =====
        case '{
              ($rest: WriterBuilder[T])
                .updatePartial(${ BuilderField(f) })
                .fromRoot[to]($updater)
            } =>
          val op = WriterMacroOperation.UpdatePartialFromRoot(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ None },
            fun = updater.asTerm,
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== update partial from root with rename =====
        case '{
              ($rest: WriterBuilder[T])
                .updatePartial(${ BuilderField(f) })
                .withRename($rename)
                .fromRoot[to]($updater)
            } =>
          val op = WriterMacroOperation.UpdatePartialFromRoot(
            tpe = TypeRepr.of[T],
            field = f.name,
            name = '{ Some($rename) },
            fun = updater.asTerm,
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== add =====
        case '{ ($rest: WriterBuilder[T]).add($field).apply[to]($updater) } =>
          val op: WriterMacroOperation = WriterMacroOperation.Add(
            tpe = TypeRepr.of[T],
            field = field,
            fun = updater.asTerm,
            to = TypeRepr.of[to]
          )
          loop(rest, config, op +: operations)

        // ===== FieldStyle =====
        case '{ ($rest: WriterBuilder[T]).fieldStyle($style) } =>
          val cfg: Expr[WriterDerivationConfig] = '{
            (${ config }: WriterDerivationConfig).withFieldStyle($style)
          }
          loop(rest, config = cfg, operations)

        // ===== NOPE =====
        case _ => report.errorAndAbort(s"unknown tree: ${expr.asTerm.show}")
      }
    }

    val (config, operations) =
      loop(expr, config = emptyWriterConfig, operations = Seq())
    MacroWriteDescription(TypeRepr.of[T], config, operations)
  }

  private def checkOperations(operations: Seq[WriterMacroOperation]): Unit = {
    // TODO
  }
}
