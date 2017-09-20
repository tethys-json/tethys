package tethys.derivation

import tethys.commons.LowPriorityInstance
import tethys.derivation.impl.derivation.AutoDerivationMacro
import tethys.{JsonObjectWriter, JsonReader}

import scala.language.experimental.macros

trait AutoDerivation {
  implicit def jsonWriterMaterializer[A]: LowPriorityInstance[JsonObjectWriter[A]] = macro AutoDerivationMacro.jsonWriter[A]
  implicit def jsonReaderMaterializer[A]: LowPriorityInstance[JsonReader[A]] = macro AutoDerivationMacro.jsonReader[A]
}
