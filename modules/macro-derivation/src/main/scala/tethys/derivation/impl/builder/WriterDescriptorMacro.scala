package tethys.derivation.impl.builder

import tethys.core.writers.builder.{WriterBuilder, WriterDescription}
import tethys.derivation.impl.{BaseMacroDefinitions, MacroUtils}

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 23.04.17.
  */
object WriterDescriptorMacro {
  def simpleDescription[A: c.WeakTypeTag](c: blackbox.Context)(builder: c.Expr[WriterBuilder[A]]): c.Expr[WriterDescription[A]] = {
    new WriterDescriptorMacroImpl[c.type](c).simpleDescription[A](builder)
  }

  private class WriterDescriptorMacroImpl[C <: blackbox.Context](val c: C) extends WriteBuilderUtils
    with MacroUtils
    with BaseMacroDefinitions {

    import c.universe._

    def simpleDescription[A: WeakTypeTag](builder: Expr[WriterBuilder[A]]): Expr[WriterDescription[A]] = {
      val description = extractSimpleDescription(builder.tree)
      checkOperations(description.operations)
      c.Expr[WriterDescription[A]] {
        c.untypecheck {
          q"$description"
        }
      }
    }

    private def extractSimpleDescription(tree: Tree): MacroWriteDescription = tree match {
      // ===== ROOT =====
      case q"$builder.apply[${tpe: Tree}]()" =>
        MacroWriteDescription(tpe.tpe, Seq())

      // ===== remove =====
      case q"${rest: Tree}.remove[${tpe: Tree}](${f: BuilderField})" =>
        val description = extractSimpleDescription(rest)
        description.copy(operations = description.operations :+
          BuilderMacroOperation.Remove(description.tpe, f.name)
        )

      case q"${rest: Tree}.remove[$tpe](${field: Tree})" =>
        abort(s"remove function should be simple getter from first level like '_.a', but '${show(field)}' found")

      // ===== update =====
      case q"${rest: Tree}.update[${a: Tree}](${f: BuilderField}).apply[${b: Tree}](${updater: Tree})" =>
        val description = extractSimpleDescription(rest)
        description.copy(operations = description.operations :+
          BuilderMacroOperation.Update(description.tpe, f.name, updater, a.tpe, b.tpe)
        )


      case q"${rest: Tree}.update[$tpe](${field: Tree}).apply[$_]($_)" =>
        abort(s"field of update function should be simple getter from first level like '_.a', but '${show(field)}' found")

      // ===== add =====
      case q"${rest: Tree}.add(${f: String}).apply[${a: Tree}](${updater: Tree})" =>
        val description = extractSimpleDescription(rest)
        description.copy(operations = description.operations :+
          BuilderMacroOperation.Add(description.tpe, f, updater, a.tpe)
        )

      case q"${rest: Tree}.updatePartial[${a: Tree}](${f: BuilderField}).apply[$_](${updater: Tree})" =>
        val description = extractSimpleDescription(rest)
        description.copy(operations = description.operations :+
          BuilderMacroOperation.UpdatePartial(description.tpe, f.name, updater, a.tpe)
        )

      // ===== NOPE =====
      case _ => abort(s"unknown tree: ${show(tree)}")
    }

    private def checkOperations(operations: Seq[BuilderMacroOperation]): Unit = {
      //TODO
    }
  }
}
