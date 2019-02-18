package tethys.derivation.builder

case class ReaderDerivationConfig(fieldStyle: Option[FieldStyle] = None)

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
    final case class ExtractFieldValue(field: String, from: Seq[Field[_]], fun: Any) extends BuilderOperation
    final case class ExtractFieldReader(field: String, from: Seq[Field[_]], fun: Any) extends BuilderOperation
  }
}
