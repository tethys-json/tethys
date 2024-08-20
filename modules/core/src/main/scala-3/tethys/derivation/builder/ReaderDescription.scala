package tethys.derivation.builder

import tethys.FieldStyle

@deprecated("Use ReaderBuilder[A] instead")
case class ReaderDerivationConfig() {
  def withFieldStyle(fieldStyle: FieldStyle): ReaderDerivationConfig = this
  def withFieldStyle(fieldStyle: tethys.derivation.builder.FieldStyle): ReaderDerivationConfig = this
  def strict: ReaderDerivationConfig = this
}

@deprecated("Use ReaderBuilder[A] instead")
object ReaderDerivationConfig {
  def empty: ReaderDerivationConfig = ReaderDerivationConfig()
  def withFieldStyle(fieldStyle: FieldStyle): ReaderDerivationConfig = empty
  def withFieldStyle(fieldStyle: tethys.derivation.builder.FieldStyle): ReaderDerivationConfig = empty
  def strict: ReaderDerivationConfig = empty
}

@deprecated("Use ReaderBuilder[A] instead")
case class ReaderDescription[A](config: ReaderDerivationConfig, operations: Seq[ReaderDescription.BuilderOperation])

object ReaderDescription {
  sealed trait Field[A]
  object Field {
    final case class ClassField[A](name: String) extends Field[A]
    final case class RawField[A](name: String) extends Field[A]
  }


  sealed trait BuilderOperation

  object BuilderOperation {
    final case class ExtractFieldAs[B, C](field: String, fun: B => C) extends BuilderOperation
    final case class ExtractFieldValue(field: String, from: Seq[Field[?]], fun: Any) extends BuilderOperation
    final case class ExtractFieldReader(field: String, from: Seq[Field[?]], fun: Any) extends BuilderOperation
  }
}
