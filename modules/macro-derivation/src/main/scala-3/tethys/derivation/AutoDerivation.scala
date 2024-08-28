package tethys.derivation

import tethys.{JsonObjectWriter, JsonReader}
import tethys.commons.LowPriorityInstance
import scala.deriving.Mirror

@deprecated("Auto derivation is deprecated and will be removed in future versions. Use `derives` instead")
trait AutoDerivation {
  implicit inline def jsonWriterMaterializer[T: Mirror.Of]: LowPriorityInstance[JsonObjectWriter[T]] =
    LowPriorityInstance[JsonObjectWriter[T]](JsonObjectWriter.derived[T])

  implicit inline def jsonReaderMaterializer[T: Mirror.ProductOf]: LowPriorityInstance[JsonReader[T]] =
    LowPriorityInstance[JsonReader[T]](JsonReader.derived[T])
}
