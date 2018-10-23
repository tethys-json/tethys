package tethys.derivation.impl.derivation

import tethys.derivation.builder.{ReaderDescription, WriterDescription}
import tethys.{JsonObjectWriter, JsonReader}

import scala.reflect.macros.blackbox

class SemiautoDerivationMacro(val c: blackbox.Context) extends WriterDerivation with ReaderDerivation {

  import c.universe._

  def simpleJsonWriter[A: WeakTypeTag]: Expr[JsonObjectWriter[A]] = {
    val tpe = weakTypeOf[A]
    val clazz = classSym(tpe)
    if (isCaseClass(tpe)) {
      deriveWriter[A]
    } else if (clazz.isSealed) {
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
    if (isCaseClass(tpe)) {
      deriveReader[A]
    } else {
      fail(s"Can't auto derive JsonWriter[$tpe]")
    }
  }

  def describedJsonReader[A: WeakTypeTag](description: Expr[ReaderDescription[A]]): Expr[JsonReader[A]] = {
    val tpe = weakTypeOf[A]
    if (isCaseClass(tpe)) {
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


