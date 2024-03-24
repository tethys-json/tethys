package tethys.derivation.impl.derivation

import tethys.derivation.impl.LoggingUtils

import scala.reflect.macros.blackbox

trait DerivationUtils extends LoggingUtils {
  val c: blackbox.Context
  import c.universe._

  protected def showError: Boolean = false

  protected def fail(msg: String): Nothing = abort(msg)

  protected def collectDistinctSubtypes(tpe: Type): List[Type] = {
    val baseClass = classSym(tpe)
    val baseArgs = tpe.dealias.typeArgs

    val tpes = collectSubclasses(baseClass).map { sym =>
      def substituteArgs: List[Type] = {
        val subst = c.internal.thisType(sym).baseType(baseClass).typeArgs
        sym.typeParams.map { param =>
          val paramTpe = param.asType.toType
          val index = subst.indexWhere(_ =:= paramTpe)
          if(index != -1) baseArgs(index)
          else fail(s"$sym contains additional type parameter that can't be derived in compile time")
        }
      }

      appliedType(sym, substituteArgs)
    }

    tpes.foldLeft(List.empty[Type]) {
      case (acc, t) =>
        if(!acc.exists(_ =:= t)) t :: acc
        else acc
    }
  }

  protected def collectSubclasses(classSym: ClassSymbol): List[ClassSymbol] = {
    classSym.knownDirectSubclasses.toList.flatMap { child0 =>
      val child = child0.asClass
      child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
      if (child.isSealed && (child.isAbstract || child.isTrait)) collectSubclasses(child)
      else List(child)
    }
  }

  protected def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass)
      fail(s"$sym is not a class or trait")

    val classSym = sym.asClass
    classSym.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

    classSym
  }
}
