package tethys.derivation.builder

import tethys.derivation.builder.WriterDescription.BuilderOperation

case class WriterDescription[A](operations: Seq[BuilderOperation[A]])

object WriterDescription {
  trait BuilderOperation[A]

  object BuilderOperation {
    case class Remove[A](field: String) extends BuilderOperation[A]
    case class Update[A, B, C](field: String, name: String, fun: B => C) extends BuilderOperation[A]
    case class UpdateFromRoot[A, C](field: String, name: String, fun: A => C) extends BuilderOperation[A]
    case class UpdatePartial[A, B](field: String, name: String, fun: PartialFunction[B, Any]) extends BuilderOperation[A]
    case class UpdatePartialFromRoot[A](field: String, name: String, fun: PartialFunction[A, Any]) extends BuilderOperation[A]
    case class Add[A, B](field: String, fun: A => B) extends BuilderOperation[A]
  }
}