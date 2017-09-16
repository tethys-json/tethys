package tethys.derivation.impl.derivation

import tethys.{JsonReader, JsonWriter}
import tethys.commons.LowPriorityInstance

import scala.reflect.macros.blackbox

object AutoDerivationMacro {
  def jsonWriter[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[LowPriorityInstance[JsonWriter[A]]] = {
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

    def jsonWriter[A: WeakTypeTag]: Expr[LowPriorityInstance[JsonWriter[A]]] = {
      val tpe = weakTypeOf[A]
      val clazz = classSym(tpe)
      val instance = {
        if (isCaseClass(tpe)) {
          deriveWriter[A]
        } else if (clazz.isSealed) {
          deriveWriterForSealedClass[A]
        } else {
          fail(s"Can't auto derive JsonWriter[$tpe]")
        }
      }

      c.Expr[LowPriorityInstance[JsonWriter[A]]] {
        c.untypecheck {
          q"new ${weakTypeOf[LowPriorityInstance[JsonWriter[A]]]}($instance)"
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
