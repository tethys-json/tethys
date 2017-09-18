package tethys.derivation.impl.derivation

import tethys.derivation.impl.builder.WriteBuilderUtils
import tethys.derivation.impl.{BaseMacroDefinitions, CaseClassUtils}
import tethys.writers.JsonObjectWriter
import tethys.writers.tokens.TokenWriter

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

trait WriterDerivation
  extends WriteBuilderUtils
    with CaseClassUtils
    with BaseMacroDefinitions
    with DerivationUtils {

  val c: blackbox.Context
  import c.universe._

  private val valueTerm = TermName("value")
  private val tokenWriterType = tq"${typeOf[TokenWriter]}"
  private val tokenWriterTerm = TermName("tokenWriter")
  private val jsonWriterType = tq"$tethysPack.JsonWriter"
  private val jsonObjectWriterType = tq"$writersPack.JsonObjectWriter"

  def deriveWriter[A: WeakTypeTag]: Expr[JsonObjectWriter[A]] = {
    val description = MacroWriteDescription(
      tpe = weakTypeOf[A],
      operations = Seq()
    )
    deriveWriter[A](description)
  }

  def deriveWriterForSealedClass[A: WeakTypeTag]: Expr[JsonObjectWriter[A]] = {
    val tpe = weakTypeOf[A]
    implicit val context: WriterContext = new WriterContext

    val subClassesCases = collectDistinctSubtypes(tpe).sortBy(_.typeSymbol.fullName).map { subtype =>
      val term = TermName(c.freshName("sub"))
      cq"$term: $subtype => ${context.provideWriter(subtype)}.writeValues($term, $tokenWriterTerm)"
    }

    if(subClassesCases.isEmpty) fail(s"${tpe.typeSymbol} has no known direct subclass")

    c.Expr[JsonObjectWriter[A]] {
      c.untypecheck {
        q"""
           new $jsonObjectWriterType[$tpe] {
              ${provideThisObjectWriterImplicit(tpe)}

              ..${context.objectWriters}

              override def writeValues($valueTerm: $tpe, $tokenWriterTerm: $tokenWriterType): Unit = {
                $valueTerm match { case ..$subClassesCases }
              }
           } : $jsonObjectWriterType[$tpe]
         """
      }
    }
  }

  def deriveWriter[A: WeakTypeTag](description: MacroWriteDescription): Expr[JsonObjectWriter[A]] = {
    val tpe = description.tpe

    implicit val context: WriterContext = new WriterContext

    val fields = deriveFields(description)
    c.Expr[JsonObjectWriter[A]] {
      c.untypecheck {
        q"""
           new $jsonObjectWriterType[$tpe] {
              ${provideThisWriterImplicit(tpe)}

              ..${context.writers}

              ..${context.functions}

              override def writeValues($valueTerm: $tpe, $tokenWriterTerm: $tokenWriterType): Unit = {
                ..$fields
              }
           } : $jsonObjectWriterType[$tpe]
         """
      }
    }
  }

  private def provideThisWriterImplicit(tpe: Type): Tree = {
    c.typecheck(q"implicitly[$jsonWriterType[$tpe]]", silent = true) match {
      case EmptyTree =>
        val thisWriterTerm = TermName(c.freshName("thisWriter"))
        q"implicit private[this] def $thisWriterTerm: $jsonWriterType[$tpe] = this"
      case _ => EmptyTree
    }
  }

  private def provideThisObjectWriterImplicit(tpe: Type): Tree = {
    c.typecheck(q"implicitly[$jsonObjectWriterType[$tpe]]", silent = true) match {
      case EmptyTree =>
        val thisWriterTerm = TermName(c.freshName("thisWriter"))
        q"implicit private[this] def $thisWriterTerm: $jsonObjectWriterType[$tpe] = this"
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
        q"private[this] lazy val $name = implicitly[$jsonWriterType[$tpe]]"
    }.toSeq

    def objectWriters: Seq[Tree] = writersMapping.map {
      case (tpe, name) =>
        q"private[this] lazy val $name = implicitly[$jsonObjectWriterType[$tpe]]"
    }.toSeq

    @tailrec
    private def unwrapType(tpe: Type): Type = tpe match {
      case ConstantType(const) => unwrapType(const.tpe)
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
}
