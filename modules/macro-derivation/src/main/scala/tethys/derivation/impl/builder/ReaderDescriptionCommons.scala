package tethys.derivation.impl.builder

import tethys.derivation.builder.{ReaderBuilder, ReaderDerivationConfig, ReaderDescription}

import scala.reflect.macros.blackbox

trait ReaderDescriptionCommons extends ReaderBuilderUtils {
  val c: blackbox.Context
  import c.universe._

  def convertReaderBuilder[A: WeakTypeTag](builder: Expr[ReaderBuilder[A]]): Expr[ReaderDescription[A]] = {
    val description = extractDescription(builder.tree)

    c.Expr[ReaderDescription[A]] {
      c.untypecheck {
        q"$description"
      }
    }
  }

  protected lazy val emptyReaderConfig: Expr[ReaderDerivationConfig] = c.Expr[ReaderDerivationConfig](c.untypecheck(
    q"tethys.derivation.builder.ReaderDerivationConfig.empty"
  ))

  private def extractDescription(tree: Tree): ReaderMacroDescription = tree match {
    // ===== ROOT =====
    case q"ReaderBuilder.apply[$_]" =>
      ReaderMacroDescription(emptyReaderConfig, Seq())

    case q"$_.ReaderBuilder.apply[$_]" =>
      ReaderMacroDescription(emptyReaderConfig, Seq())

    // ===== FieldAs =====
    case q"${rest: Tree}.extract[${tpe: Tree}](${f: BuilderField}).as[${as: Tree}].apply(${fun: Tree})" =>
      val description = extractDescription(rest)
      description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldAs(
        f.name, tpe.tpe, as.tpe, fun
      ))

    // ===== FieldValue =====
    case q"${rest: Tree}.extract[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).apply(${fun: Tree})" =>
      val description = extractDescription(rest)
      description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldValue(
        f.name, fs, fun
      ))

    case q"${rest: Tree}.extract[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).and[..$_](..${ands: Seq[Field]}).apply(${fun: Tree})" =>
      val description = extractDescription(rest)
      description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldValue(
        f.name, fs ++ ands, fun
      ))

    // ===== FieldReader =====
    case q"${rest: Tree}.extractReader[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).apply(${fun: Tree})" =>
      val description = extractDescription(rest)
      description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldReader(
        f.name, fs, fun
      ))

    case q"${rest: Tree}.extractReader[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).and[..$_](..${ands: Seq[Field]}).apply(${fun: Tree})" =>
      val description = extractDescription(rest)
      description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldReader(
        f.name, fs ++ ands, fun
      ))

    // ===== FieldStyle =====
    case q"${rest: Tree}.fieldStyle(${style: Tree})" =>
      val description = extractDescription(rest)
      description.copy(config = c.Expr[ReaderDerivationConfig](
        q"${description.config.tree}.withFieldStyle($style)"
      ))

    // ===== isStrict =====
    case q"${rest: Tree}.strict" =>
      val description = extractDescription(rest)
      description.copy(config = c.Expr[ReaderDerivationConfig](
        q"${description.config.tree}.strict"
      ))

    // ===== NOPE =====
    case _ =>
      abort(s"Unknown builder tree: $tree")
  }
}
