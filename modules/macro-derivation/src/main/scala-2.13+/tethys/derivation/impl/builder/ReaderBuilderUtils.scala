package tethys.derivation.impl.builder

import tethys.derivation.builder.ReaderDerivationConfig
import tethys.derivation.impl.MacroUtils

import scala.reflect.macros.blackbox

trait ReaderBuilderUtils extends MacroUtils {
  val c: blackbox.Context
  import c.universe._

  case class ReaderMacroDescription(config: c.Expr[ReaderDerivationConfig], operations: Seq[ReaderMacroOperation])

  sealed trait Field {
    def name: String
    def tpe: Type
  }
  object Field {
    final case class ClassField(name: String, tpe: Type) extends Field
    final case class RawField(name: String, tpe: Type) extends Field
  }

  sealed trait ReaderMacroOperation {
    def field: String
  }
  object ReaderMacroOperation {
    final case class ExtractFieldAs(field: String, tpe: Type, as: Type, fun: Tree) extends ReaderMacroOperation
    final case class ExtractFieldValue(field: String, from: Seq[Field], fun: Tree) extends ReaderMacroOperation
    final case class ExtractFieldReader(field: String, from: Seq[Field], fun: Tree) extends ReaderMacroOperation
  }

  implicit lazy val readerMacroDescriptionLiftable: Liftable[ReaderMacroDescription] = Liftable[ReaderMacroDescription] {
    case ReaderMacroDescription(config, operations) =>
      q"$buildersPack.ReaderDescription(${config.tree} ,_root_.scala.Seq(..$operations))"
  }

  implicit lazy val readerMacroDescriptionUnliftable: Unliftable[ReaderMacroDescription] = Unliftable[ReaderMacroDescription] {
    case q"$_.ReaderDescription.apply[$_](${config: Tree} ,$_.Seq.apply[$_](..${operations: Seq[ReaderMacroOperation]}))" =>
      ReaderMacroDescription(c.Expr[ReaderDerivationConfig](c.untypecheck(config)), operations)
  }

  implicit lazy val fieldLiftable: Liftable[Field] = Liftable[Field] {
    case Field.ClassField(name, tpe) =>
      q"$buildersPack.ReaderDescription.Field.ClassField[$tpe]($name)"

    case Field.RawField(name, tpe) =>
      q"$buildersPack.ReaderDescription.Field.RawField[$tpe]($name)"
  }

  implicit lazy val fieldUnliftable: Unliftable[Field] = Unliftable[Field] {
    case q"$_.ReaderDescription.Field.ClassField.apply[${tpe: Tree}](${name: String})" =>
      Field.ClassField(name, tpe.tpe)

    case q"$_.ReaderDescription.Field.RawField.apply[${tpe: Tree}](${name: String})" =>
      Field.RawField(name, tpe.tpe)

    case q"${f: BuilderField}" =>
      Field.ClassField(f.name, f.tpe)

    case q"$_.ReaderFieldStringOps(${name: String}).as[${tpe: Tree}]" =>
      Field.RawField(name, tpe.tpe)

    case q"$_.ReaderFieldSymbolOps(scala.Symbol.apply(${name: String})).as[${tpe: Tree}]" =>
      Field.RawField(name, tpe.tpe)
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
