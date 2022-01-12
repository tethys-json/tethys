package tethys.derivation.impl.derivation

import tethys.JsonObjectWriter
import tethys.derivation.builder.{FieldStyle, WriterDerivationConfig}
import tethys.derivation.impl.builder.{WriteBuilderUtils, WriterBuilderCommons}
import tethys.derivation.impl.{BaseMacroDefinitions, CaseClassUtils}
import tethys.writers.tokens.TokenWriter

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

trait WriterDerivation
  extends WriterBuilderCommons
    with CaseClassUtils
    with BaseMacroDefinitions
    with DerivationUtils {

  val c: blackbox.Context
  import c.universe._

  private val valueTerm = TermName("value")
  private val tokenWriterType = tq"${typeOf[TokenWriter]}"
  private val tokenWriterTerm = TermName("tokenWriter")
  private val jsonWriterType = tq"$tethysPack.JsonWriter"
  private val jsonObjectWriterType = tq"$tethysPack.JsonObjectWriter"

  def deriveWriter[A: WeakTypeTag]: Expr[JsonObjectWriter[A]] = {
    val description = MacroWriteDescription(
      tpe = weakTypeOf[A],
      config = emptyWriterConfig,
      operations = Seq()
    )
    deriveWriter[A](description)
  }

  def deriveWriterForSealedClass[A: WeakTypeTag]: Expr[JsonObjectWriter[A]] = {
    deriveWriterForSealedClass[A](emptyWriterConfig)
  }

  def deriveWriterForSealedClass[A: WeakTypeTag](config: c.Expr[WriterDerivationConfig]): Expr[JsonObjectWriter[A]] = {
    val tpe = weakTypeOf[A]

    val types = collectDistinctSubtypes(tpe).sortBy(_.typeSymbol.fullName)

    if(types.isEmpty) fail(s"${tpe.typeSymbol} has no known direct subclass")
    else {

      val terms = types.map(_ => TermName(c.freshName()))

      val writers = types.zip(terms).map {
        case (subtype, term) =>
          q"private[this] lazy val $term = implicitly[$jsonObjectWriterType[$subtype]]"
      }

      val subClassesCases = types.zip(terms).map {
        case (subtype, writer) =>
          val term = TermName(c.freshName("sub"))
          val discriminatorTerm = TermName(c.freshName("discriminator"))
          val typeName = subtype.typeSymbol.asClass.name.decodedName.toString.trim
          cq"""$term: $subtype => {
               $writer.writeValues($term, $tokenWriterTerm)
               ${config.tree}.discriminator.foreach { $discriminatorTerm: String =>
                 implicitly[$jsonWriterType[String]].write($discriminatorTerm, $typeName, $tokenWriterTerm)
               }
          }"""
      }

      c.Expr[JsonObjectWriter[A]] {
        c.untypecheck {
          q"""
           new $jsonObjectWriterType[$tpe] {
              ${provideThisWriterImplicit(tpe)}

              ..$writers

              override def writeValues($valueTerm: $tpe, $tokenWriterTerm: $tokenWriterType): Unit = {
                $valueTerm match { case ..$subClassesCases }
              }
           } : $jsonObjectWriterType[$tpe]
         """
        }
      }
    }
  }

  def deriveWriter[A: WeakTypeTag](description: MacroWriteDescription): Expr[JsonObjectWriter[A]] = {
    val tpe = description.tpe
    val config = scala.util.Try(c.eval(description.config)).getOrElse(c.eval(description.config))
    val writerFields = applyFieldStyle(config.fieldStyle)
        .andThen(applyDescriptionOperations(description.operations))
        .apply(makeFields[A])

    val (typeWriters, writerTrees) = allocateWriters(writerFields)
    val functions = allocateFunctions(writerFields)

    val fieldTrees = writerFields.map {
      case SimpleWriterField(_, jsonName, fieldTpe, extractor) =>
        val valueTree = extractor match {
          case InlineExtract(tree) =>
            tree
          case FunctionExtractor(name, InlineExtract(tree), _, _, _) =>
            q"$name.apply($tree)"
        }
        val writerTerm = typeWriters.find(_._1 =:= fieldTpe).get._2
        q"$writerTerm.write(${jsonName.tree}, $valueTree, $tokenWriterTerm)"

      case PartialExtractedField(_, jsonName, argExtractor, cases) =>
        val valueTree = argExtractor match {
          case InlineExtract(tree) =>
            tree
          case FunctionExtractor(name, InlineExtract(tree), _, _, _) =>
            q"$name.apply($tree)"
        }

        val resultCases = cases.map {
          case CaseDef(d, g, body) =>
            val fieldTpe = unwrapType(body.tpe.finalResultType)
            val writerTerm = typeWriters.find(_._1 =:= fieldTpe).get._2
            CaseDef(d, g, q"$writerTerm.write(${jsonName.tree}, $body, $tokenWriterTerm)")
        }

        q"$valueTree match { case ..$resultCases }"
    }

    c.Expr[JsonObjectWriter[A]] {
      c.untypecheck {
        q"""
           new $jsonObjectWriterType[$tpe] {
              ${provideThisWriterImplicit(tpe)}

              ..$writerTrees

              ..$functions

              override def writeValues($valueTerm: $tpe, $tokenWriterTerm: $tokenWriterType): Unit = {
                ..$fieldTrees
              }
           } : $jsonObjectWriterType[$tpe]
         """
      }
    }
  }

  sealed trait Extractor
  case class InlineExtract(tree: Tree) extends Extractor
  case class FunctionExtractor(name: TermName, arg: InlineExtract, from: Type, to: Type, body: Tree) extends Extractor

  private sealed trait WriterField {
    def name: String
  }
  private case class SimpleWriterField(name: String, jsonName: Expr[String], tpe: Type, extractor: Extractor) extends WriterField
  private case class PartialExtractedField(name: String, jsonName: Expr[String], argExtractor: Extractor, cases: List[CaseDef]) extends WriterField
  private def makeFields[A: WeakTypeTag]: List[WriterField] = {
    val classDef = caseClassDefinition[A]

    classDef.fields.map { field =>
      SimpleWriterField(
        name = field.name,
        jsonName = c.Expr[String](q"${field.name}"),
        tpe = field.tpe,
        extractor = InlineExtract(q"$valueTerm.${TermName(field.name)}")
      )
    }
  }

  private def applyFieldStyle(fieldStyle: Option[FieldStyle]): List[WriterField] => List[WriterField] = writerFields => {
    fieldStyle.fold(writerFields) { style =>
      writerFields.map {
        case field: SimpleWriterField => field.copy(jsonName = c.Expr[String](q"${style.applyStyle(field.name)}"))
        case field => field
      }
    }
  }

  private def applyDescriptionOperations(operations: Seq[BuilderMacroOperation]): List[WriterField] => List[WriterField] = writerFields => {
    def mapField(fields: List[WriterField], name: String)(f: SimpleWriterField => WriterField): List[WriterField] = {
      fields.map {
        case field: SimpleWriterField if field.name == name => f(field)
        case field => field
      }
    }

    operations.foldLeft(writerFields) {
      case (fields, operation) =>
        operation match {
          case BuilderMacroOperation.Remove(_, field) =>
            fields.filterNot(_.name == field)

          case BuilderMacroOperation.Update(_, field, name, fun, from, to) =>
            mapField(fields, field)(f => SimpleWriterField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              tpe = to,
              extractor = FunctionExtractor(
                name = TermName(c.freshName()),
                arg = InlineExtract(q"$valueTerm.${TermName(field)}"),
                from = from,
                to = to,
                body = fun
              )
            ))

          case BuilderMacroOperation.UpdateFromRoot(tpe, field, name, fun, to) =>
            mapField(fields, field)(f => SimpleWriterField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              tpe = to,
              extractor = FunctionExtractor(
                name = TermName(c.freshName()),
                arg = InlineExtract(q"$valueTerm"),
                from = tpe,
                to = to,
                body = fun
              )
            ))

          case BuilderMacroOperation.UpdatePartial(_, field, name, fun, from) =>
            mapField(fields, field)(f => PartialExtractedField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              argExtractor = InlineExtract(q"$valueTerm.${TermName(field)}"),
              cases = fun match {
                case q"{ case ..$cases }" => cases.asInstanceOf[Seq[CaseDef]].toList
              }
            ))

          case BuilderMacroOperation.UpdatePartialFromRoot(_, field, name, fun) =>
            mapField(fields, field)(f => PartialExtractedField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              argExtractor = InlineExtract(q"$valueTerm"),
              cases = fun match {
                case q"{ case ..$cases }" => cases.asInstanceOf[Seq[CaseDef]].toList
              }
            ))

          case BuilderMacroOperation.Add(tpe, field, fun, to) =>
            fields ::: List(SimpleWriterField(
              name = "__---nope---__",
              jsonName = field,
              tpe = to,
              extractor = FunctionExtractor(
                name = TermName(c.freshName()),
                arg = InlineExtract(q"$valueTerm"),
                from = tpe,
                to = to,
                body = fun
              )
            ))
        }
    }
  }

  private def allocateWriters(writerFields: List[WriterField]): (List[(Type, TermName)], List[Tree]) = {
    val types = writerFields.flatMap {
      case SimpleWriterField(_, _, tpe, _) => List(tpe)
      case PartialExtractedField(_, _, _, cases) => cases.map {
        case CaseDef(_, _, body) => unwrapType(body.tpe.finalResultType)
      }
    }

    types.foldLeft(List[(Type, TermName)](), List[Tree]()) {
      case ((terms, trees), tpe) if !terms.exists(_._1 =:= tpe) =>
        val term = TermName(c.freshName())
        val tree = {
          if (tpe =:= typeOf[Nothing]) q"private[this] lazy val $term = $writersPack.EmptyWriters.emptyWriter[Nothing]"
          else q"private[this] lazy val $term = implicitly[$jsonWriterType[$tpe]]"
        }
        ((tpe, term) :: terms, tree :: trees)

      case (res, _) => res
    }
  }

  private def allocateFunctions(writerFields: List[WriterField]): List[Tree] = {
    writerFields.flatMap {
      case SimpleWriterField(_, _, _, FunctionExtractor(name, _, from, to, body)) =>
        q"private[this] val $name: $from => $to = $body" :: Nil
      case PartialExtractedField(_, _, FunctionExtractor(name, _, from, to, body), _) =>
        q"private[this] val $name: $from => $to = $body" :: Nil
      case _ =>
        Nil
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

  @tailrec
  private def unwrapType(tpe: Type): Type = tpe match {
    case ConstantType(const) => unwrapType(const.tpe)
    case _ => tpe
  }
}
