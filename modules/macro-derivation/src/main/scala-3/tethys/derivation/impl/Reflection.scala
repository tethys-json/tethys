package tethys.derivation.impl

import scala.quoted.*

import tethys.readers.JsonReaderDefaultValue
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}

trait Reflection {
  given Quotes = context
  val context: Quotes
  import context.reflect.*

  extension (underlying: TypeRepr) {
    def searchInlineJsonReader: Term = searchInline[JsonReader]

    def searchInlineJsonWriter: Term = searchInline[JsonWriter]

    def getWrite2Method: Term = underlying.searchInlineJsonWriter.selectWrite2Method

    def getWrite3Method: Term = underlying.searchInlineJsonWriter.selectWrite3Method

    def searchInlineJsonObjectWriter: Term = searchInline[JsonObjectWriter]

    def searchJsonObjectWriter: Term = searchUnsafe[JsonObjectWriter]

    def safeSearchJsonObjectWriter: Option[Term] = searchSafe[JsonObjectWriter]

    def searchJsonReaderDefaultValue: Term = searchUnsafe[JsonReaderDefaultValue]

    def getDealiasFullName: String = underlying.dealias.typeSymbol.fullName

    private def searchInline[F[_]: Type]: Term = underlying.asType match {
      case '[t] => '{ scala.compiletime.summonInline[F[t]] }.asTerm
    }

    private def searchSafe[F[_]: Type]: Option[Term] = underlying.asType match {
      case '[t] => Expr.summon[F[t]].map(_.asTerm)
    }

    private def searchUnsafe[F[_]: Type]: Term = underlying.asType match {
      case '[t] => Expr.summon[F[t]]
        .getOrElse(report.errorAndAbort(s"Can't find implicit for ${Type.show[F[t]]}"))
        .asTerm
    }
  }

  extension (underlying: Term) {
    def selectFirstMethod(methodName: String) =
      underlying.select(underlying.tpe.typeSymbol.getFirstMethod(methodName))

    def selectField(fieldName: String): Term =
      underlying.select(underlying.tpe.typeSymbol.fieldMember(fieldName))

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
    def selectWriteValuesMethod: Term =
      underlying.selectFirstMethod("writeValues")

    def selectWrite2Method: Term =
      selectWriteMethod(2)

    def selectWrite3Method: Term =
      selectWriteMethod(3)

    private def selectWriteMethod(argsCount: Int) =
      underlying.tpe.typeSymbol
        .findMethod("write")(_.signature.paramSigs.length == argsCount)
        .map(underlying.select)
        .get
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

  def collectDistinctSubtypes(baseTpe: TypeRepr): List[TypeRepr] = {
    def collectSubclasses(parent: Symbol): List[Symbol] = {
      parent.children.flatMap { child =>
        if (child.flags.is(Flags.Sealed) && (child.flags.is(Flags.Trait) || child.flags.is(Flags.Abstract)))
          collectSubclasses(child)
        else
          List(child)
      }
    }

    val baseSym = baseTpe.typeSymbol
    val baseArgs = baseTpe.typeArgs
    val children = collectSubclasses(baseSym)

    def substituteArgs(childTpe: TypeRepr, childSym: Symbol): TypeRepr = {
      val subst = childTpe.baseType(baseSym).typeArgs

      val args = childSym.typeMembers.map { param =>
        val paramTpe = TypeIdent(param).tpe
        val index = subst.indexWhere(_ =:= paramTpe)
        if (index != -1) baseArgs(index)
        else
          report.errorAndAbort(s"$childSym contains additional type parameter that can't be derived in compile time")
      }

      childTpe.appliedTo(args)
    }

    val tpes = children.map { childSym =>
      if (childSym.isType)
        substituteArgs(TypeIdent(childSym).tpe, childSym)
      else
        Ref(childSym).tpe
    }

    tpes.foldLeft(List.empty[TypeRepr]) { case (acc, t) =>
      if (!acc.exists(_ =:= t)) t :: acc
      else acc
    }
  }
}
