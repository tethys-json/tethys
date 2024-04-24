package tethys.derivation

import tethys.JsonWriter

private[derivation]
case class JsonWriterProductConfigParsed[T](
    fields: List[JsonWriterProductConfigParsed.Field[T, ?]]
)


private[derivation]
object JsonWriterProductConfigParsed:
  case class Field[T, F](label: String, function: T => F, writer: JsonWriter[F])