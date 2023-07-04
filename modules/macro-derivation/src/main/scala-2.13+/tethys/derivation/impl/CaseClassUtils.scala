package tethys.derivation.impl

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 23.04.17.
  */
trait CaseClassUtils extends LoggingUtils {
  val c: blackbox.Context
  import c.universe._

  case class CaseClassDefinition(tpe: Type, fields: List[CaseClassField])
  case class CaseClassField(name: String, tpe: Type)

  def caseClassDefinition[A: WeakTypeTag]: CaseClassDefinition = caseClassDefinition(weakTypeOf[A])

  def caseClassDefinition(tpe: Type): CaseClassDefinition = {
    val ctor = getConstructor(tpe)
    CaseClassDefinition(
      tpe = tpe,
      fields = ctor.paramLists.head.map(constructorParameterToCaseClassField(tpe))
    )
  }

  def isCaseClass[A: WeakTypeTag]: Boolean = isCaseClass(weakTypeOf[A])

  def isCaseClass(tpe: Type): Boolean = {
    tpe.typeSymbol.isClass &&
      (tpe.typeSymbol.asClass.isCaseClass ||
        tpe.member(TermName("copy")).isMethod &&
        tpe <:< weakTypeOf[Product])
  }

  private def getConstructor(tpe: Type): MethodSymbol = {
    tpe.decls.collectFirst {
      case s: MethodSymbol if s.isPrimaryConstructor => s
    }.getOrElse {
      abort(s"Type '${tpe.typeSymbol.name.decodedName.toString} doesn't have main constructor")
    }
  }

  private def constructorParameterToCaseClassField(tpe: Type)(param: Symbol): CaseClassField = {
    val possibleRealType = tpe.decls.collectFirst {
      case s if s.name == param.name => s.typeSignatureIn(tpe).finalResultType
    }
    CaseClassField(
      name = param.name.decodedName.toString,
      tpe = possibleRealType.getOrElse(param.typeSignatureIn(tpe))
    )
  }
}
