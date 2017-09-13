package tethys.core.writers.builder

case class WriterDescription[A](operations: Seq[BuilderOperation[A]])