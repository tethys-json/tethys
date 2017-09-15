package tethys.derivation.impl.derivation

import tethys.JsonWriter
import tethys.derivation.builder.WriterDescription

import scala.reflect.macros.blackbox

object SemiautoDerivationMacro {

  def simpleJsonWriter[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[JsonWriter[A]] = {
    new SemiautoDerivationMacroImpl[c.type](c).simpleJsonWriter[A]
  }

  def describedJsonWriter[A: c.WeakTypeTag](c: blackbox.Context)(description: c.Expr[WriterDescription[A]]): c.Expr[JsonWriter[A]] = {
    new SemiautoDerivationMacroImpl[c.type](c).describedJsonWriter[A](description)
  }

  private class SemiautoDerivationMacroImpl[C <: blackbox.Context](val c: C)
    extends WriterDerivation {

    import c.universe._

    def simpleJsonWriter[A: WeakTypeTag]: Expr[JsonWriter[A]] = {
      val tpe = weakTypeOf[A]
      val clazz = classSym(tpe)
      if (isCaseClass(tpe)) {
        deriveWriter[A]
      } else if(clazz.isSealed) {
        deriveWriterForSealedClass[A]
      } else {
        abort(s"Can't auto derive JsonWriter[$tpe]")
      }
    }

    def describedJsonWriter[A: WeakTypeTag](description: Expr[WriterDescription[A]]): Expr[JsonWriter[A]] = {
      val tpe = weakTypeOf[A]
      if (!isCaseClass(tpe)) {
        abort(s"Can't auto derive JsonWriter[$tpe]")
      } else {
        deriveWriter[A](unliftMacroDescription(description))
      }
    }

    private def unliftMacroDescription[A: WeakTypeTag](description: Expr[WriterDescription[A]]): MacroWriteDescription = {
      description.tree match {
        case Untyped(q"${description: MacroWriteDescription}") => description
      }
    }
  }
}


