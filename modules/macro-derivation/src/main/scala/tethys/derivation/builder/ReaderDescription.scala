package tethys.derivation.builder

case class ReaderDescription[A](operations: Seq[ReaderDescription.BuilderOperation])

object ReaderDescription {
  final case class Field[A](name: String)

  sealed trait BuilderOperation

  object BuilderOperation {
    final case class ExtractFieldAs[B, C](field: String, fun: (B) => C) extends BuilderOperation
    final case class ExtractFieldValue(field: String, from: Seq[Field[_]], fun: Any) extends BuilderOperation
    final case class ExtractFieldReader(field: String, from: Seq[Field[_]], fun: Any) extends BuilderOperation
  }
}
