package tethys.derivation.builder

import tethys.derivation.builder.WriterDescription.BuilderOperation

case class WriterDerivationConfig(fieldStyle: Option[FieldStyle], discriminator: Option[String] = None) {
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig = this.copy(fieldStyle = Some(fieldStyle))
  def withDiscriminator(discriminator: String): WriterDerivationConfig = this.copy(discriminator = Some(discriminator))
}

object WriterDerivationConfig {
  def empty: WriterDerivationConfig = WriterDerivationConfig(None)
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig = empty.copy(fieldStyle = Some(fieldStyle))
}

case class WriterDescription[A](config: WriterDerivationConfig, operations: Seq[BuilderOperation[A]])

object WriterDescription {
  trait BuilderOperation[A]

  object BuilderOperation {
    case class Remove[A](field: String) extends BuilderOperation[A]
    case class Update[A, B, C](field: String, name: Option[String], fun: B => C) extends BuilderOperation[A]
    case class UpdateFromRoot[A, C](field: String, name: Option[String], fun: A => C) extends BuilderOperation[A]
    case class UpdatePartial[A, B](field: String, name: Option[String], fun: PartialFunction[B, Any]) extends BuilderOperation[A]
    case class UpdatePartialFromRoot[A](field: String, name: Option[String], fun: PartialFunction[A, Any]) extends BuilderOperation[A]
    case class Add[A, B](field: String, fun: A => B) extends BuilderOperation[A]
  }
}