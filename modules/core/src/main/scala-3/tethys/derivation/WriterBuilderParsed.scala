package tethys.derivation

import tethys.JsonWriter

private[derivation]
object WriterBuilderParsed:
  case class Field[T, F](label: String, function: T => F, writer: JsonWriter[F])