package tethys.derivation.impl.builder

import tethys.derivation.impl.MacroUtils

import scala.reflect.macros.blackbox

trait ReaderBuilderUtils extends MacroUtils {
  val c: blackbox.Context
  import c.universe._

  case class ReaderMacroDescription(operations: Seq[ReaderMacroOperation])

  final case class Field(name: String, tpe: Type)

  sealed trait ReaderMacroOperation {
    def field: String
  }
  object ReaderMacroOperation {
    final case class ExtractFieldAs(field: String, tpe: Type, as: Type, fun: Tree) extends ReaderMacroOperation
    final case class ExtractFieldValue(field: String, from: Seq[Field], fun: Tree) extends ReaderMacroOperation
    final case class ExtractFieldReader(field: String, from: Seq[Field], fun: Tree) extends ReaderMacroOperation
  }

  implicit lazy val readerMacroDescriptionLiftable: Liftable[ReaderMacroDescription] = Liftable[ReaderMacroDescription] {
    case ReaderMacroDescription(operations) =>
      q"$buildersPack.ReaderDescription(_root_.scala.Seq(..$operations))"
  }

  implicit lazy val readerMacroDescriptionUnliftable: Unliftable[ReaderMacroDescription] = Unliftable[ReaderMacroDescription] {
    case q"$_.ReaderDescription.apply[$_]($_.Seq.apply[$_](..${operations: Seq[ReaderMacroOperation]}))" =>
      ReaderMacroDescription(operations)
  }

  implicit lazy val fieldLiftable: Liftable[Field] = Liftable[Field] {
    case Field(name, tpe) =>
      q"$buildersPack.ReaderDescription.Field[$tpe]($name)"
  }

  implicit lazy val fieldUnliftable: Unliftable[Field] = Unliftable[Field] {
    case q"$_.ReaderDescription.Field.apply[${tpe: Tree}](${name: String})" =>
      Field(name, tpe.tpe)

    case q"${f: BuilderField}" =>
      Field(f.name, f.tpe)

    case q"$_.ReaderFieldStringOps(${name: String}).as[${tpe: Tree}]" =>
      Field(name, tpe.tpe)

    case q"$_.ReaderFieldSymbolOps(scala.Symbol.apply(${name: String})).as[${tpe: Tree}]" =>
      Field(name, tpe.tpe)
  }

  implicit lazy val readerMacroOperationLiftable: Liftable[ReaderMacroOperation] = Liftable[ReaderMacroOperation] {
    case ReaderMacroOperation.ExtractFieldAs(field, tpe, as, fun) =>
      q"$buildersPack.ReaderDescription.BuilderOperation.ExtractFieldAs[$as, $tpe]($field, $fun)"

    case ReaderMacroOperation.ExtractFieldValue(field, from, fun) =>
      q"$buildersPack.ReaderDescription.BuilderOperation.ExtractFieldValue($field, _root_.scala.Seq(..$from), $fun)"

    case ReaderMacroOperation.ExtractFieldReader(field, from, fun) =>
      q"$buildersPack.ReaderDescription.BuilderOperation.ExtractFieldReader($field, _root_.scala.Seq(..$from), $fun)"
  }


  implicit lazy val readerMacroOperationUnliftable: Unliftable[ReaderMacroOperation] = Unliftable[ReaderMacroOperation] {
    case q"$_.ReaderDescription.BuilderOperation.ExtractFieldAs.apply[${as: Tree}, ${tpe: Tree}](${field: String}, ${fun: Tree})" =>
      ReaderMacroOperation.ExtractFieldAs(field, tpe.tpe, as.tpe, fun)

    case q"$_.ReaderDescription.BuilderOperation.ExtractFieldValue.apply(${field: String}, $_.Seq.apply[$_](..${from: Seq[Field]}), ${fun: Tree})" =>
      ReaderMacroOperation.ExtractFieldValue(field, from, fun)

    case q"$_.ReaderDescription.BuilderOperation.ExtractFieldReader.apply(${field: String}, $_.Seq.apply[$_](..${from: Seq[Field]}), ${fun: Tree})" =>
      ReaderMacroOperation.ExtractFieldReader(field, from, fun)
  }
}
