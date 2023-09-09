package tethys.derivation.builder

import tethys.derivation.builder.WriterDescription.BuilderOperation

case class WriterDerivationConfig(
    fieldStyle: Option[FieldStyle],
    discriminator: Option[String] = None
) {
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig =
    this.copy(fieldStyle = Some(fieldStyle))
  def withDiscriminator(discriminator: String): WriterDerivationConfig =
    this.copy(discriminator = Some(discriminator))
}

object WriterDerivationConfig {
  def empty: WriterDerivationConfig = WriterDerivationConfig(None)
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig =
    empty.copy(fieldStyle = Some(fieldStyle))
  def withDiscriminator(discriminator: String): WriterDerivationConfig =
    empty.copy(discriminator = Some(discriminator))
}

case class WriterDescription[A](
    config: WriterDerivationConfig,
    operations: Seq[BuilderOperation[A]]
)

object WriterDescription {
  trait BuilderOperation[A]

  object BuilderOperation {
    case class Remove[T](field: String) extends BuilderOperation[T]
    case class Update[T, From, To](
        field: String,
        name: Option[String],
        fun: From => To
    ) extends BuilderOperation[T]
    case class UpdateFromRoot[T, To](
        field: String,
        name: Option[String],
        fun: T => To
    ) extends BuilderOperation[T]
    case class UpdatePartial[T, From, To](
        field: String,
        name: Option[String],
        fun: PartialFunction[From, To]
    ) extends BuilderOperation[T]
    case class UpdatePartialFromRoot[T, To](
        field: String,
        name: Option[String],
        fun: PartialFunction[T, To]
    ) extends BuilderOperation[T]
    case class Add[T, To](field: String, fun: T => To)
        extends BuilderOperation[T]
  }
}
