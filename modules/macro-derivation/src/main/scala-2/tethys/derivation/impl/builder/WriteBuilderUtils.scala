package tethys.derivation.impl.builder

import tethys.derivation.builder.WriterDerivationConfig
import tethys.derivation.impl.MacroUtils

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 24.04.17.
  */
trait WriteBuilderUtils extends MacroUtils {
  val c: blackbox.Context
  import c.universe._

  case class MacroWriteDescription(tpe: Type, config: c.Expr[WriterDerivationConfig], operations: Seq[BuilderMacroOperation])

  implicit lazy val macroWriteDescriptionLiftable: Liftable[MacroWriteDescription] = {
    Liftable[MacroWriteDescription] {
      case MacroWriteDescription(tpe, config, operations) =>
        q"$buildersPack.WriterDescription[$tpe](${config.tree}, _root_.scala.Seq(..$operations))"
    }
  }

  implicit lazy val macroWriteDescriptionUnliftable: Unliftable[MacroWriteDescription] = {
    Unliftable[MacroWriteDescription] {
      case q"$pack.WriterDescription.apply[${tpe: Tree}](${config: Tree}, $col.Seq.apply[$_](..${operations: Seq[BuilderMacroOperation]}))" =>
        MacroWriteDescription(tpe.tpe, c.Expr[WriterDerivationConfig](c.untypecheck(config)), operations)
    }
  }

  sealed trait BuilderMacroOperation
  object BuilderMacroOperation {
    case class Remove(tpe: Type, field: String) extends BuilderMacroOperation
    case class Update(tpe: Type, field: String, name: Option[Expr[String]], fun: Tree, from: Type, to: Type) extends BuilderMacroOperation
    case class UpdateFromRoot(tpe: Type, field: String, name: Option[Expr[String]], fun: Tree, to: Type) extends BuilderMacroOperation
    case class UpdatePartial(tpe: Type, field: String, name: Option[Expr[String]], fun: Tree, from: Type) extends BuilderMacroOperation
    case class UpdatePartialFromRoot(tpe: Type, field: String, name: Option[Expr[String]], fun: Tree) extends BuilderMacroOperation
    case class Add(tpe: Type, field: Expr[String], fun: Tree, to: Type) extends BuilderMacroOperation
  }

  implicit lazy val builderMacroOperationLiftable: Liftable[BuilderMacroOperation] = Liftable[BuilderMacroOperation] {
    case BuilderMacroOperation.Remove(tpe, field) =>
      q"$buildersPack.WriterDescription.BuilderOperation.Remove.apply[$tpe]($field)"

    case BuilderMacroOperation.Update(tpe, field, name, fun, from, to) =>
      q"$buildersPack.WriterDescription.BuilderOperation.Update.apply[$tpe, $from, $to]($field, ${name.map(_.tree)}, $fun)"

    case BuilderMacroOperation.UpdateFromRoot(tpe, field, name, fun, to) =>
      q"$buildersPack.WriterDescription.BuilderOperation.UpdateFromRoot.apply[$tpe, $to]($field, ${name.map(_.tree)}, $fun)"

    case BuilderMacroOperation.UpdatePartial(tpe, field, name, fun, to) =>
      q"$buildersPack.WriterDescription.BuilderOperation.UpdatePartial.apply[$tpe, $to]($field, ${name.map(_.tree)}, $fun)"

    case BuilderMacroOperation.UpdatePartialFromRoot(tpe, field, name, fun) =>
      q"$buildersPack.WriterDescription.BuilderOperation.UpdatePartialFromRoot.apply[$tpe]($field, ${name.map(_.tree)}, $fun)"

    case BuilderMacroOperation.Add(tpe, field, fun, to) =>
      q"$buildersPack.WriterDescription.BuilderOperation.Add.apply[$tpe, $to](${field.tree}, $fun)"
  }

  implicit lazy val builderMacroOperationUnliftable: Unliftable[BuilderMacroOperation] = Unliftable[BuilderMacroOperation] {
    case q"$pack.BuilderOperation.Remove.apply[${tpe: Tree}](${field: String})" =>
      BuilderMacroOperation.Remove(tpe.tpe, field)

    case q"$pack.BuilderOperation.Update.apply[${tpe: Tree}, ${from: Tree}, ${to: Tree}](${field: String}, ${name: Option[Tree]}, ${fun: Tree})" =>
      BuilderMacroOperation.Update(tpe.tpe, field, name.map(c.Expr[String](_)), fun, from.tpe, to.tpe)

    case q"$pack.BuilderOperation.UpdateFromRoot.apply[${tpe: Tree}, ${to: Tree}](${field: String}, ${name: Option[Tree]}, ${fun: Tree})" =>
      BuilderMacroOperation.UpdateFromRoot(tpe.tpe, field, name.map(c.Expr[String](_)), fun, to.tpe)

    case q"$pack.BuilderOperation.UpdatePartial.apply[${tpe: Tree}, ${from: Tree}](${field: String}, ${name: Option[Tree]}, ${fun: Tree})" =>
      BuilderMacroOperation.UpdatePartial(tpe.tpe, field, name.map(c.Expr[String](_)), fun, from.tpe)

    case q"$pack.BuilderOperation.UpdatePartialFromRoot.apply[${tpe: Tree}](${field: String}, ${name: Option[Tree]}, ${fun: Tree})" =>
      BuilderMacroOperation.UpdatePartialFromRoot(tpe.tpe, field, name.map(c.Expr[String](_)), fun)

    case q"$pack.BuilderOperation.Add.apply[${tpe: Tree}, ${to: Tree}](${field: Tree}, ${fun: Tree})" =>
      BuilderMacroOperation.Add(tpe.tpe, c.Expr(field), fun, to.tpe)
  }
}
