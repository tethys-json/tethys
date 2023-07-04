package tethys.derivation

import scala.quoted.*

import tethys.{JsonObjectWriter, JsonReader}
import tethys.commons.LowPriorityInstance
import tethys.derivation.impl.derivation.AutoDerivationMacro

trait AutoDerivation {
  implicit inline def jsonWriterMaterializer[T]: LowPriorityInstance[JsonObjectWriter[T]] =
    ${ AutoDerivation.jsonWriterMaterializer[T] }
    
  implicit inline def jsonReaderMaterializer[T]: LowPriorityInstance[JsonReader[T]] =
    ${ AutoDerivation.jsonReaderMaterializer[T] }
}

private[this] object AutoDerivation {
  def jsonWriterMaterializer[T: Type](using Quotes): Expr[LowPriorityInstance[JsonObjectWriter[T]]] =
    new AutoDerivationMacro(quotes).simpleJsonWriter[T]

  def jsonReaderMaterializer[T: Type](using Quotes): Expr[LowPriorityInstance[JsonReader[T]]] =
    new AutoDerivationMacro(quotes).simpleJsonReader[T]
}