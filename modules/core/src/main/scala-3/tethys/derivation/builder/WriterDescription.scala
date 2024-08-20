package tethys.derivation.builder

import tethys.FieldStyle
import tethys.derivation.builder.WriterDescription.BuilderOperation

@deprecated("Use WriterBuilder[A] instead")
case class WriterDerivationConfig(fieldStyle: Option[FieldStyle], discriminator: Option[String] = None) {
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig = this
  def withFieldStyle(fieldStyle: tethys.derivation.builder.FieldStyle): WriterDerivationConfig = this
  def withDiscriminator(discriminator: String): WriterDerivationConfig = this
}

object WriterDerivationConfig {
  def empty: WriterDerivationConfig = WriterDerivationConfig(None)
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig = empty
  def withFieldStyle(fieldStyle: tethys.derivation.builder.FieldStyle): WriterDerivationConfig = empty
  def withDiscriminator(discriminator: String): WriterDerivationConfig = empty
}

@deprecated("Use WriterBuilder[A] instead")
case class WriterDescription[A](config: WriterDerivationConfig, operations: Seq[BuilderOperation[A]])

object WriterDescription {
  trait BuilderOperation[A]

  object BuilderOperation {
    case class Remove[T](field: String) extends BuilderOperation[T]
    case class Update[T, From, To](field: String, name: Option[String], fun: From => To) extends BuilderOperation[T]
    case class UpdateFromRoot[T, To](field: String, name: Option[String], fun: T => To) extends BuilderOperation[T]
    case class UpdatePartial[T, From, To](field: String, name: Option[String], fun: PartialFunction[From, To]) extends BuilderOperation[T]
    case class UpdatePartialFromRoot[T, To](field: String, name: Option[String], fun: PartialFunction[T, To]) extends BuilderOperation[T]
    case class Add[T, To](field: String, fun: T => To) extends BuilderOperation[T]
  }
}