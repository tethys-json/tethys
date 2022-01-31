package tethys.derivation.impl

import scala.compiletime.summonInline
import scala.quoted.*

import tethys.readers.JsonReaderDefaultValue
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}

trait Reflection {
  given Quotes = context
  val context: Quotes
  import context.reflect.*

  extension (underlying: TypeRepr) {
    def searchJsonReader: Term = search[JsonReader]

    def searchJsonWriter: Term = search[JsonWriter]

    def getWriteMethod: Term = underlying.searchJsonWriter.selectWriteMethod

    def searchJsonObjectWriter: Term = search[JsonObjectWriter]

    def getWriteValuesMethod: Term = underlying.searchJsonObjectWriter.selectFirstMethod("writeValues")

    def searchJsonReaderDefaultValue: Term =
      underlying.asType match {
        case '[t] =>
          Expr.summon[JsonReaderDefaultValue[t]] match {
            case Some(instance) => instance.asTerm
            case None => report.errorAndAbort(s"Can't find implicit for ${Type.show[JsonReaderDefaultValue[t]]}")
          }
      }

    def getTypeArgs: List[TypeRepr] =
      underlying.widenTermRefByName.dealias match {
        case AppliedType(_, args) => args
        case _                    => Nil
      }

    private def search[F[_]: Type]: Term = underlying.asType match {
      case '[t] => '{ summonInline[F[t]] }.asTerm
    }
  }

  extension (underlying: Term) {
    def selectFirstMethod(methodName: String) =
      underlying.select(underlying.tpe.typeSymbol.getFirstMethod(methodName))

    def selectField(fieldName: String): Term =
      underlying.select(underlying.tpe.typeSymbol.fieldMember(fieldName))

    def selectWriteMethod: Term =
      underlying.tpe.typeSymbol
        .findMethod("write")(_.signature.paramSigs.length == 3)
        .map(underlying.select)
        .get

    def foldOption(ifEmpty: Term)(f: Term => Term): Term = {
      underlying.tpe.asType match {
        case '[Option[_]] => {
          val underlyingSym = underlying.tpe.typeSymbol
          val isDefinedMethod = underlyingSym.getFirstMethod("isDefined")
          val getMethod = underlyingSym.getFirstMethod("get")
          If(
            underlying.select(isDefinedMethod),
            f(underlying.select(getMethod)),
            ifEmpty
          )
        }
        case _ => report.errorAndAbort(s"Field ${underlying.show} is not Option[_]")
      }
    }
  }

  extension [T: Type](underlying: Expr[Option[T]]) {
    def getOrElse(ifEmpty: Expr[T]): Expr[T] =
      underlying.asTerm.foldOption(ifEmpty.asTerm)(identity).asExprOf[T]
  }

  extension (underlying: Symbol) {
    def getFirstMethod(methodName: String): Symbol =
      underlying
        .methodMember(methodName)
        .headOption
        .getOrElse(report.errorAndAbort(s"Can't find method $methodName in ${underlying.name}"))

    def findMethod(methodName: String)(pattern: Symbol => Boolean): Option[Symbol] =
      underlying.methodMember(methodName).find(pattern)
  }

  case class BuilderField(name: String, tpe: TypeRepr)

  object BuilderField {
    def unapply(expr: Expr[Any]): Option[BuilderField] =
      unapply(expr.asTerm)

    def unapply(term: Term): Option[BuilderField] =
      term match {
        case Lambda(List(ValDef(name, _, _)), body @ SelectChain(b)) if b.chain.size == 2 && name == b.chain.head =>
          Some(BuilderField(b.chain(1), body.tpe.widen))
        case _ => None
      }
  }

  case class SelectChain(chain: Seq[String])

  object SelectChain {
    def unapply(term: Term): Option[SelectChain] =
      term match {
        case Ident(name) => Some(SelectChain(Seq(name)))
        case select: Select =>
          def selectAllNames(s: Tree): Seq[String] = s match {
            case Select(rest, name) => selectAllNames(rest) :+ name
            case Ident(name)        => Seq(name)
          }

          Some(SelectChain(selectAllNames(select)))
        case _ => None
      }
  }
}
