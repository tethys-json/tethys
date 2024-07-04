package tethys.derivation

import tethys.JsonWriter

private[derivation]
case class WriterBuilderParsed[T](
    fields: List[WriterBuilderParsed.Field[T, ?]]
)


private[derivation]
object WriterBuilderParsed:
  case class Field[T, F](label: String, function: T => F, writer: JsonWriter[F])