package tethys.derivation.impl.derivation

import tethys.derivation.builder.{ReaderDescription, WriterDescription}
import tethys.{JsonObjectWriter, JsonReader}

import scala.reflect.macros.blackbox

object SemiautoDerivationMacro {

  def simpleJsonWriter[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[JsonObjectWriter[A]] = {
    new SemiautoDerivationMacroImpl[c.type](c).simpleJsonWriter[A]
  }

  def describedJsonWriter[A: c.WeakTypeTag](c: blackbox.Context)(description: c.Expr[WriterDescription[A]]): c.Expr[JsonObjectWriter[A]] = {
    new SemiautoDerivationMacroImpl[c.type](c).describedJsonWriter[A](description)
  }

  def simpleJsonReader[A: c.WeakTypeTag](c: blackbox.Context): c.Expr[JsonReader[A]] = {
    new SemiautoDerivationMacroImpl[c.type](c).simpleJsonReader[A]
  }

  def describedJsonReader[A: c.WeakTypeTag](c: blackbox.Context)(description: c.Expr[ReaderDescription[A]]): c.Expr[JsonReader[A]] = {
    new SemiautoDerivationMacroImpl[c.type](c).describedJsonReader[A](description)
  }

  private class SemiautoDerivationMacroImpl[C <: blackbox.Context](val c: C)
    extends WriterDerivation
      with ReaderDerivation {

    import c.universe._

    def simpleJsonWriter[A: WeakTypeTag]: Expr[JsonObjectWriter[A]] = {
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

    def describedJsonWriter[A: WeakTypeTag](description: Expr[WriterDescription[A]]): Expr[JsonObjectWriter[A]] = {
      val tpe = weakTypeOf[A]
      if (!isCaseClass(tpe)) {
        abort(s"Can't auto derive JsonWriter[$tpe]")
      } else {
        deriveWriter[A](unliftWriterMacroDescription(description))
      }
    }

    def simpleJsonReader[A: WeakTypeTag]: Expr[JsonReader[A]] = {
      val tpe = weakTypeOf[A]
      if(isCaseClass(tpe)) {
        deriveReader[A]
      } else {
        fail(s"Can't auto derive JsonWriter[$tpe]")
      }
    }

    def describedJsonReader[A: WeakTypeTag](description: Expr[ReaderDescription[A]]): Expr[JsonReader[A]] = {
      val tpe = weakTypeOf[A]
      if(isCaseClass(tpe)) {
        deriveReader[A](unliftReaderMacroDescription(description))
      } else {
        fail(s"Can't auto derive JsonWriter[$tpe]")
      }
    }

    private def unliftWriterMacroDescription[A: WeakTypeTag](description: Expr[WriterDescription[A]]): MacroWriteDescription = {
      description.tree match {
        case Untyped(q"${description: MacroWriteDescription}") => description
      }
    }

    private def unliftReaderMacroDescription[A: WeakTypeTag](description: Expr[ReaderDescription[A]]): ReaderMacroDescription = {
      description.tree match {
        case Untyped(q"${description: ReaderMacroDescription}") => description
      }
    }
  }
}


