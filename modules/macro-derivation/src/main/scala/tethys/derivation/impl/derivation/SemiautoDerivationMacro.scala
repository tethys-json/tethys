package tethys.derivation.impl.derivation

import tethys.derivation.builder._
import tethys.derivation.impl.builder.WriterBuilderCommons
import tethys.{JsonObjectWriter, JsonReader}

import scala.reflect.macros.blackbox

class SemiautoDerivationMacro(val c: blackbox.Context)
  extends WriterDerivation
  with ReaderDerivation
  with WriterBuilderCommons {

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

  def jsonWriterWithBuilder[A: WeakTypeTag](builder: Expr[WriterBuilder[A]]): Expr[JsonObjectWriter[A]] = {
    val description = convertWriterBuilder[A](builder)
    describedJsonWriter[A](c.Expr[WriterDescription[A]](c.typecheck(description.tree)))
  }

  def jsonWriterWithConfig[A: WeakTypeTag](config: Expr[WriterDerivationConfig]): Expr[JsonObjectWriter[A]] = {
    val tpe = weakTypeOf[A]
    val clazz = classSym(tpe)

    if (isCaseClass(tpe)) {
      deriveWriter[A](
        MacroWriteDescription(
          tpe = tpe,
          config = c.Expr[WriterDerivationConfig](c.untypecheck(config.tree)),
          operations = Seq.empty
        )
      )
    } else if (clazz.isSealed) {
      deriveWriterForSealedClass[A](c.Expr[WriterDerivationConfig](c.untypecheck(config.tree)))
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
      fail(s"Can't auto derive JsonReader[$tpe]")
    }
  }

  def jsonReaderWithBuilder[A: WeakTypeTag](builder: Expr[ReaderBuilder[A]]): Expr[JsonReader[A]] = {
    val description = convertReaderBuilder[A](builder)
    describedJsonReader[A](c.Expr[ReaderDescription[A]](c.typecheck(description.tree)))
  }

  def describedJsonReader[A: WeakTypeTag](description: Expr[ReaderDescription[A]]): Expr[JsonReader[A]] = {
    val tpe = weakTypeOf[A]
    if (isCaseClass(tpe)) {
      deriveReader[A](unliftReaderMacroDescription(description))
    } else {
      fail(s"Can't auto derive JsonReader[$tpe]")
    }
  }

  def jsonReaderWithConfig[A: WeakTypeTag](config: Expr[ReaderDerivationConfig]): Expr[JsonReader[A]] = {
    val tpe = weakTypeOf[A]
    if (isCaseClass(tpe)) {
      deriveReader[A](ReaderMacroDescription(
        config = c.Expr[ReaderDerivationConfig](c.untypecheck(config.tree)),
        operations = Seq()
      ))
    } else {
      fail(s"Can't auto derive JsonReader[$tpe]")
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


