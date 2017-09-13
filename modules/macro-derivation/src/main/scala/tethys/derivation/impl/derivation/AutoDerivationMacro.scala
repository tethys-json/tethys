package tethys.derivation.impl.derivation

import tethys.core.commons.LowPriorityInstance
import tethys.core.writers.JsonWriter

import scala.reflect.macros.blackbox

object AutoDerivationMacro {
  def jsonWriter[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[LowPriorityInstance[JsonWriter[A]]] = {
    new AutoDerivationMacroImpl[c.type ](c).jsonWriter[A]
  }

  private class AutoDerivationMacroImpl[C <: blackbox.Context](val c: C)
    extends WriterDerivation {

    import c.universe._

    def jsonWriter[A: WeakTypeTag]: Expr[LowPriorityInstance[JsonWriter[A]]] = {
      val tpe = weakTypeOf[A]
      val clazz = classSym(tpe)
      val instance = {
        if (isCaseClass(tpe)) {
          deriveWriter[A]
        } else if(clazz.isSealed) {
          deriveWriterForSealedClass[A]
        } else {
          abort(s"Can't auto derive JsonWriter[$tpe]")
        }
      }

      val lowPriorityInstance = tq"${weakTypeOf[LowPriorityInstance[JsonWriter[A]]]}"
      c.Expr[LowPriorityInstance[JsonWriter[A]]] {
        q"new $lowPriorityInstance($instance)"
      }
    }
  }
}
