package tethys.derivation.impl.derivation

import tethys.JsonReader
import tethys.commons.LowPriorityInstance
import tethys.writers.JsonObjectWriter

import scala.reflect.macros.blackbox

object AutoDerivationMacro {
  def jsonWriter[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[LowPriorityInstance[JsonObjectWriter[A]]] = {
    new AutoDerivationMacroImpl[c.type](c).jsonWriter[A]
  }

  def jsonReader[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[LowPriorityInstance[JsonReader[A]]] = {
    new AutoDerivationMacroImpl[c.type](c).jsonReader[A]
  }

  private class AutoDerivationMacroImpl[C <: blackbox.Context](val c: C)
    extends WriterDerivation
      with ReaderDerivation {

    import c.universe._

    override protected def showError: Boolean = true

    def jsonWriter[A: WeakTypeTag]: Expr[LowPriorityInstance[JsonObjectWriter[A]]] = {
      val tpe = weakTypeOf[A]
      val clazz = classSym(tpe)
      val instance: Expr[JsonObjectWriter[A]] = {
        if (isCaseClass(tpe)) {
          deriveWriter[A]
        } else if (clazz.isSealed) {
          deriveWriterForSealedClass[A]
        } else {
          fail(s"Can't auto derive JsonWriter[$tpe]")
        }
      }

      c.Expr[LowPriorityInstance[JsonObjectWriter[A]]] {
        c.untypecheck {
          q"new ${weakTypeOf[LowPriorityInstance[JsonObjectWriter[A]]]}($instance)"
        }
      }
    }

    def jsonReader[A: WeakTypeTag]: Expr[LowPriorityInstance[JsonReader[A]]] = {
      val tpe = weakTypeOf[A]
      if(isCaseClass(tpe)) {
        val instance = deriveReader[A]
        c.Expr[LowPriorityInstance[JsonReader[A]]] {
          c.untypecheck {
            q"new ${weakTypeOf[LowPriorityInstance[JsonReader[A]]]}($instance)"
          }
        }
      } else {
        fail(s"Can't auto derive JsonWriter[$tpe]")
      }
    }
  }

}
