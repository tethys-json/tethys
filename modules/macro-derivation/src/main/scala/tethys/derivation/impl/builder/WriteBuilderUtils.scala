package tethys.derivation.impl.builder

import tethys.derivation.impl.MacroUtils

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 24.04.17.
  */
trait WriteBuilderUtils extends MacroUtils {
  val c: blackbox.Context
  import c.universe._

  case class MacroWriteDescription(tpe: Type, operations: Seq[BuilderMacroOperation])

  implicit lazy val simpleMacroWriteDescriptionLiftable: Liftable[MacroWriteDescription] = {
    Liftable[MacroWriteDescription] {
      case MacroWriteDescription(tpe, operations) =>
        q"$buildersPack.WriterDescription[$tpe](_root_.scala.Seq(..$operations))"
    }
  }

  implicit lazy val simpleMacroWriteDescriptionUnliftable: Unliftable[MacroWriteDescription] = {
    Unliftable[MacroWriteDescription] {
      case q"$pack.WriterDescription.apply[${tpe: Tree}]($col.Seq.apply[$_](..${operations: Seq[BuilderMacroOperation]}))" =>
        MacroWriteDescription(tpe.tpe, operations)
    }
  }

  sealed trait BuilderMacroOperation
  object BuilderMacroOperation {
    case class Remove(tpe: Type, field: String) extends BuilderMacroOperation
    case class Update(tpe: Type, field: String, fun: Tree, from: Type, to: Type) extends BuilderMacroOperation
    case class UpdatePartial(tpe: Type, field: String, fun: Tree, from: Type) extends BuilderMacroOperation
    case class Add(tpe: Type, field: String, fun: Tree, to: Type) extends BuilderMacroOperation
  }

  implicit lazy val builderMacroOperationLiftable: Liftable[BuilderMacroOperation] = Liftable[BuilderMacroOperation] {
    case BuilderMacroOperation.Remove(tpe, field) =>
      q"$buildersPack.WriterDescription.BuilderOperation.Remove.apply[$tpe]($field)"

    case BuilderMacroOperation.Update(tpe, field, fun, from, to) =>
      q"$buildersPack.WriterDescription.BuilderOperation.Update.apply[$tpe, $from, $to]($field, $fun)"

    case BuilderMacroOperation.UpdatePartial(tpe, field, fun, from) =>
      q"$buildersPack.WriterDescription.BuilderOperation.UpdatePartial.apply[$tpe, $from]($field, $fun)"

    case BuilderMacroOperation.Add(tpe, field, fun, to) =>
      q"$buildersPack.WriterDescription.BuilderOperation.Add.apply[$tpe, $to]($field, $fun)"
  }

  implicit lazy val builderMacroOperationUnliftable: Unliftable[BuilderMacroOperation] = Unliftable[BuilderMacroOperation] {
    case q"$pack.BuilderOperation.Remove.apply[${tpe: Tree}](${field: String})" =>
      BuilderMacroOperation.Remove(tpe.tpe, field)

    case q"$pack.BuilderOperation.Update.apply[${tpe: Tree}, ${from: Tree}, ${to: Tree}](${field: String}, ${fun: Tree})" =>
      BuilderMacroOperation.Update(tpe.tpe, field, fun, from.tpe, to.tpe)

    case q"$pack.BuilderOperation.UpdatePartial.apply[${tpe: Tree}, ${from: Tree}](${field: String}, ${fun: Tree})" =>
      BuilderMacroOperation.UpdatePartial(tpe.tpe, field, fun, from.tpe)

    case q"$pack.BuilderOperation.Add.apply[${tpe: Tree}, ${to: Tree}](${field: String}, ${fun: Tree})" =>
      BuilderMacroOperation.Add(tpe.tpe, field, fun, to.tpe)
  }
}
