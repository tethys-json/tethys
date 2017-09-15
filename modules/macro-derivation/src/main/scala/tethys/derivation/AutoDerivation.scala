package tethys.derivation

import tethys.JsonWriter
import tethys.commons.LowPriorityInstance
import tethys.derivation.impl.derivation.AutoDerivationMacro

import scala.language.experimental.macros

trait AutoDerivation {
  implicit def jsonWriterMaterializer[A]: LowPriorityInstance[JsonWriter[A]] = macro AutoDerivationMacro.jsonWriter[A]
}
