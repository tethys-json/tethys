package tethys.derivation

import tethys.core.commons.LowPriorityInstance
import tethys.core.writers.JsonWriter
import tethys.derivation.impl.derivation.AutoDerivationMacro

import scala.language.experimental.macros

trait AutoDerivation {
  implicit def jsonWriterMaterializer[A]: LowPriorityInstance[JsonWriter[A]] = macro AutoDerivationMacro.jsonWriter[A]
}
