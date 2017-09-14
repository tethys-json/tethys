package tethys.derivation.impl.derivation

import tethys.core.writers.JsonWriter
import tethys.core.writers.token.TokenWriter
import tethys.derivation.impl.builder.WriteBuilderUtils
import tethys.derivation.impl.{BaseMacroDefinitions, CaseClassUtils}

import scala.collection.mutable
import scala.reflect.macros.blackbox

trait WriterDerivation extends WriteBuilderUtils with CaseClassUtils with BaseMacroDefinitions {
  val c: blackbox.Context
  import c.universe._

  private val valueTerm = TermName("value")
  private val tokenWriterType = tq"${typeOf[TokenWriter]}"
  private val tokenWriterTerm = TermName("tokenWriter")

  def deriveWriter[A: WeakTypeTag]: Expr[JsonWriter[A]] = {
    val description = MacroWriteDescription(
      tpe = weakTypeOf[A],
      operations = Seq()
    )
    deriveWriter[A](description)
  }

  def deriveWriterForSealedClass[A: WeakTypeTag]: Expr[JsonWriter[A]] = {
    val tpe = weakTypeOf[A]
    implicit val context: WriterContext = new WriterContext

    val subClassesCases = collectDistinctSubtypes(tpe).sortBy(_.typeSymbol.fullName).map { subtype =>
      val term = TermName(c.freshName("sub"))
      cq"$term: $subtype => ${context.provideWriter(subtype)}.write($term, $tokenWriterTerm)"
    }

    if(subClassesCases.isEmpty) abort(s"${tpe.typeSymbol} has no known direct subclass")

    c.Expr[JsonWriter[A]] {
      c.untypecheck {
        q"""
           new $writersPack.JsonWriter[$tpe] {
              ${providerThisWriterImplicit(tpe)}

              ..${context.writers}

              override def write($valueTerm: $tpe, $tokenWriterTerm: $tokenWriterType): Unit = {
                $valueTerm match { case ..$subClassesCases }
              }
           } : $writersPack.JsonWriter[$tpe]
         """
      }
    }
  }

  def deriveWriter[A: WeakTypeTag](description: MacroWriteDescription): Expr[JsonWriter[A]] = {
    val tpe = description.tpe

    implicit val context: WriterContext = new WriterContext

    val fields = deriveFields(description)
    c.Expr[JsonWriter[A]] {
      c.untypecheck {
        q"""
           new $writersPack.JsonWriter[$tpe] {
              ${providerThisWriterImplicit(tpe)}

              ..${context.writers}

              ..${context.functions}

              override def write($valueTerm: $tpe, $tokenWriterTerm: $tokenWriterType): Unit = {
                $tokenWriterTerm.writeStartObject()
                ..$fields
                $tokenWriterTerm.writeEndObject()
              }
           } : $writersPack.JsonWriter[$tpe]
         """
      }
    }
  }

  private def collectDistinctSubtypes(tpe: Type): List[Type] = {
    val baseClass = classSym(tpe)
    val baseArgs = tpe.dealias.typeArgs

    val tpes = collectSubclasses(baseClass).map { sym =>
      def substituteArgs: List[Type] = {
        val subst = c.internal.thisType(sym).baseType(baseClass).typeArgs
        sym.typeParams.map { param =>
          val paramTpe = param.asType.toType
          val index = subst.indexWhere(_ =:= paramTpe)
          if(index != -1) baseArgs(index)
          else abort(s"$sym contains additional type parameter that can't be derived in compile time")
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

  private def collectSubclasses(classSym: ClassSymbol): List[ClassSymbol] = {
    classSym.knownDirectSubclasses.toList.flatMap { child0 =>
      val child = child0.asClass
      child.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
      if (child.isCaseClass) List(child)
      else if (child.isSealed) collectSubclasses(child)
      else abort(s"$child is not case class or a sealed trait")
    }
  }

  private def providerThisWriterImplicit(tpe: Type): Tree = {
    c.typecheck(q"implicitly[$writersPack.JsonWriter[$tpe]]", silent = true) match {
      case EmptyTree =>
        val thisWriterTerm = TermName(c.freshName("thisWriter"))
        q"implicit private[this] def $thisWriterTerm: $writersPack.JsonWriter[$tpe] = this"
      case _ => EmptyTree
    }
  }

  private def deriveFields(description: MacroWriteDescription)
                          (implicit context: WriterContext): Seq[JsonField] = {
    def simpleGetter(name: String) = q"$valueTerm.${TermName(name)}"

    val classDef = caseClassDefinition(description.tpe)
    val fieldsData: Seq[(String, Either[Type, Tree] , Tree)] = classDef.fields.map(f => (f.name, Left(f.tpe), simpleGetter(f.name)))

    val allFields = orderOperations(description.operations).foldLeft(fieldsData) {
      case (data, o: BuilderMacroOperation.Remove) =>
        data.filterNot(_._1 == o.field)

      case (data, o: BuilderMacroOperation.Add) =>
        val fun = context.addFunction(classDef.tpe, o.to, o.fun)
        val newData = (o.field, Left(o.to), q"$fun($valueTerm)")
        if(!data.exists(_._1 == o.field)) data :+ newData
        else data.map {
          case (name, _, _) if name == o.field => newData
          case d => d
        }

      case (data, o: BuilderMacroOperation.Update) =>
        data.map {
          case (name, _, _) if name == o.field =>
            val fun = context.addFunction(o.from, o.to, o.fun)
            (name, Left(o.to), q"$fun(${simpleGetter(name)})")
          case d => d
        }

      case (data, o: BuilderMacroOperation.UpdatePartial) =>
        data.map {
          case (name, _, _) if name == o.field =>
            (name, Right(o.fun), q"${simpleGetter(name)}")
          case d => d
        }

    }

    allFields.map {
      case (name, Left(tpe), expr) => SimpleJsonField(name, context.provideWriter(tpe), expr)
      case (name, Right(fun), expr) =>
        val cases = fun match {
          case q"{ case ..$cases }" =>
            cases.map {
              case cd@CaseDef(_, _, body) =>
                context.provideWriter(body.tpe.finalResultType) -> cd
            }
        }
        PartialJsonField(name, cases, expr)
    }
  }

  private def orderOperations(operations: Seq[BuilderMacroOperation]): Seq[BuilderMacroOperation] = {
    val size = operations.length
    operations.zipWithIndex.sortBy {
      case (_: BuilderMacroOperation.Remove, i) => i - size
      case (_, i) => i
    }.map(_._1)
  }

  protected class WriterContext {
    private val writersMapping: mutable.Map[Type, TermName] = mutable.Map[Type, TermName]()
    private val funs: mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer[Tree]()

    def provideWriter(tpe: Type): TermName = {
      writersMapping.getOrElseUpdate(unwrapType(tpe), TermName(c.freshName("writer")))
    }

    def addFunction(from: Type, to: Type, body: Tree): TermName = {
      val funTerm = TermName(c.freshName("fun"))
      funs += q"private[this] lazy val $funTerm: $from => $to = $body"
      funTerm
    }

    def functions: Seq[Tree] = funs.toList
    def writers: Seq[Tree] = writersMapping.map {
      case (tpe, name) if tpe =:= typeOf[Nothing] =>
        q"private[this] lazy val $name = $writersPack.EmptyWriters.emptyWriter[Nothing]"

      case (tpe, name) =>
        q"private[this] lazy val $name = implicitly[$writersPack.JsonWriter[$tpe]]"
    }.toSeq

    private def unwrapType(tpe: Type): Type = tpe match {
      case ConstantType(const) => const.tpe
      case _ => tpe
    }
  }

  sealed trait JsonField
  case class SimpleJsonField(name: String, writer: TermName, expression: Tree) extends JsonField
  case class PartialJsonField(name: String, cases: Seq[(TermName, CaseDef)], expression: Tree) extends JsonField

  implicit val jsonFieldLiftable: Liftable[JsonField] = Liftable {
    case SimpleJsonField(name, writer, expression) =>
      q"$writer.write($name, $expression, $tokenWriterTerm)"

    case PartialJsonField(name, cases, expression) =>
      val resultCases = cases.map {
        case (writer, CaseDef(d, g, body)) =>
          CaseDef(d, g, q"$writer.write($name, $body, $tokenWriterTerm)")
      }
      q"$expression match { case ..$resultCases }"
  }

  protected def classSym(tpe: Type): ClassSymbol = {
    val sym = tpe.typeSymbol
    if (!sym.isClass)
      abort(s"$sym is not a class or trait")

    val classSym = sym.asClass
    classSym.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

    classSym
  }
}
