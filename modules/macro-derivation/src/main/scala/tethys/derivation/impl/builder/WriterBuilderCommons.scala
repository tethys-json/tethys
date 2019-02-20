package tethys.derivation.impl.builder

import tethys.derivation.builder.{WriterBuilder, WriterDerivationConfig, WriterDescription}

import scala.reflect.macros.blackbox

trait WriterBuilderCommons extends WriteBuilderUtils {
  val c: blackbox.Context
  import c.universe._

  protected def convertWriterBuilder[A: WeakTypeTag](builder: Expr[WriterBuilder[A]]): Expr[WriterDescription[A]] = {
    val description = extractSimpleDescription(builder.tree)
    checkOperations(description.operations)
    c.Expr[WriterDescription[A]] {
      c.untypecheck {
        q"$description"
      }
    }
  }

  protected lazy val emptyWriterConfig: Expr[WriterDerivationConfig] = c.Expr[WriterDerivationConfig](c.untypecheck(
    q"tethys.derivation.builder.WriterDerivationConfig.empty"
  ))

  private def extractSimpleDescription(tree: Tree): MacroWriteDescription = tree match {
    // ===== ROOT =====
    case q"WriterBuilder.apply[${tpe: Tree}]" =>
      MacroWriteDescription(tpe.tpe, emptyWriterConfig, Seq())

    case q"$_.WriterBuilder.apply[${tpe: Tree}]" =>
      MacroWriteDescription(tpe.tpe, emptyWriterConfig, Seq())

    // ===== remove =====
    case q"${rest: Tree}.remove[${tpe: Tree}](${f: BuilderField})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.Remove(description.tpe, f.name)
      )

    // ===== rename =====
    case q"${rest: Tree}.rename[${a: Tree}](${f: BuilderField})(${rename: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.Update(description.tpe, f.name, Some(c.Expr(rename)), q"identity[${a.tpe}]", a.tpe, a.tpe)
      )

    // ===== update =====
    case q"${rest: Tree}.update[${a: Tree}](${f: BuilderField}).apply[${b: Tree}](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.Update(description.tpe, f.name, None, updater, a.tpe, b.tpe)
      )

    // ===== update with rename =====
    case q"${rest: Tree}.update[${a: Tree}](${f: BuilderField}).withRename(${rename: Tree}).apply[${b: Tree}](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.Update(description.tpe, f.name, Some(c.Expr(rename)), updater, a.tpe, b.tpe)
      )

    // ===== update from root =====
    case q"${rest: Tree}.update[$_](${f: BuilderField}).fromRoot[${b: Tree}](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.UpdateFromRoot(description.tpe, f.name, None, updater, b.tpe)
      )

    // ===== update from root with rename =====
    case q"${rest: Tree}.update[$_](${f: BuilderField}).withRename(${rename: Tree}).fromRoot[${b: Tree}](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.UpdateFromRoot(description.tpe, f.name, Some(c.Expr(rename)), updater, b.tpe)
      )

    // ===== add =====
    case q"${rest: Tree}.add(${f: Tree}).apply[${a: Tree}](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.Add(description.tpe, c.Expr(f), updater, a.tpe)
      )

    // ===== update partial =====
    case q"${rest: Tree}.updatePartial[${a: Tree}](${f: BuilderField}).apply[$_](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.UpdatePartial(description.tpe, f.name, None, updater, a.tpe)
      )

    // ===== update partial with rename =====
    case q"${rest: Tree}.updatePartial[${a: Tree}](${f: BuilderField}).withRename(${rename: Tree}).apply[$_](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.UpdatePartial(description.tpe, f.name, Some(c.Expr(rename)), updater, a.tpe)
      )

    // ===== update partial from root =====
    case q"${rest: Tree}.updatePartial[${a: Tree}](${f: BuilderField}).fromRoot[$_](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.UpdatePartialFromRoot(description.tpe, f.name, None, updater)
      )

    // ===== update partial from root with rename =====
    case q"${rest: Tree}.updatePartial[${a: Tree}](${f: BuilderField}).withRename(${rename: Tree}).fromRoot[$_](${updater: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(operations = description.operations :+
        BuilderMacroOperation.UpdatePartialFromRoot(description.tpe, f.name, Some(c.Expr(rename)), updater)
      )

    // ===== FieldStyle =====
    case q"${rest: Tree}.fieldStyle(${style: Tree})" =>
      val description = extractSimpleDescription(rest)
      description.copy(config = c.Expr[WriterDerivationConfig](
        q"${description.config.tree}.withFieldStyle($style)"
      ))

    // ===== NOPE =====
    case _ => abort(s"unknown tree: ${show(tree)}")
  }

  private def checkOperations(operations: Seq[BuilderMacroOperation]): Unit = {
    //TODO
  }
}
