package tethys.derivation.impl.derivation

import tethys.commons.LowPriorityInstance
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}

import scala.reflect.macros.blackbox

class AutoDerivationMacro(val c: blackbox.Context) extends WriterDerivation with ReaderDerivation {

  import c.universe._

  override protected def showError: Boolean = true

  def jsonWriter[A: WeakTypeTag]: Expr[LowPriorityInstance[JsonObjectWriter[A]]] = {
    val tpe = weakTypeOf[A]
    val clazz = classSym(tpe)
    val instance: Expr[JsonWriter[A]] = {
      if (isRecursiveDerivation) {
        fail(s"Stop recursive derivation of JsonWriter[$tpe]")
      } else if (isCaseClass(tpe)) {
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
    if (isRecursiveDerivation) {
      fail(s"Stop recursive derivation of JsonReader[$tpe]")
    } else if (isCaseClass(tpe)) {
      val instance = deriveReader[A]
      c.Expr[LowPriorityInstance[JsonReader[A]]] {
        c.untypecheck {
          q"new ${weakTypeOf[LowPriorityInstance[JsonReader[A]]]}($instance)"
        }
      }
    } else {
      fail(s"Can't auto derive JsonReader[$tpe]")
    }
  }

  private def isRecursiveDerivation: Boolean = {
    val tpes = c.enclosingMacros.map(_.macroApplication).collect {
      case q"$_.${method: TermName}[${tt: Tree}]" => method -> tt.tpe
    }

    val counts = tpes.map {
      case (m1, t1) =>
        tpes.foldLeft(0) {
          case (count, (m2, t2)) if m1 == m2 && t1 =:= t2 => count + 1
          case (count, _) => count
        }
    }

    counts.exists(_ > 1)
  }
}
