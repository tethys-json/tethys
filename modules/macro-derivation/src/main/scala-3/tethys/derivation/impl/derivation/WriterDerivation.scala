package tethys.derivation.impl.derivation

import scala.quoted.*
import scala.compiletime.*

import tethys.derivation.builder.WriterDerivationConfig
import tethys.derivation.impl.FieldStyle
import tethys.derivation.impl.builder.{WriterBuilderCommons, WriterBuilderUtils}
import tethys.writers.EmptyWriters
import tethys.writers.tokens.TokenWriter
import tethys.{JsonObjectWriter, JsonWriter}

trait WriterDerivation extends WriterBuilderCommons {
  import context.reflect.*

  // ---------------------------------------- CASE CLASS ---------------------------------------- //
  def deriveCaseClassWriteValuesWithDescription[T: Type](value: Expr[T], tokenWriter: Expr[TokenWriter])(
      description: MacroWriteDescription
  ): Expr[Unit] = {
    val valueTerm = value.asTerm
    val (fieldStyle, _) = evalWriterConfig(description.config)

    val writerFields = applyFieldStyle(fieldStyle)
      .andThen(applyDescriptionOperations(valueTerm, description.operations))
      .apply(makeFields[T](valueTerm))

    val writerInfos = allocateWriters(writerFields)

    val fieldExprs = writerFields.map {
      case SimpleWriterField(_, jsonName, fieldTpe, extractor) =>
        val valueTerm = extractor match {
          case InlineExtract(term) =>
            term
          case FunctionExtractor(arg, _, _, func) =>
            func.selectFirstMethod("apply").appliedTo(arg.term)
        }

        val writerTerm = writerInfos.find(_._1 =:= fieldTpe.widen).get._2
        writerTerm.selectWriteMethod.appliedTo(jsonName.asTerm, valueTerm, tokenWriter.asTerm).asExprOf[Unit]

      case PartialExtractedField(_, jsonName, argExtractor, cases) =>
        val valueTerm = argExtractor match {
          case InlineExtract(term) =>
            term
          case FunctionExtractor(arg, _, _, func) =>
            func.selectFirstMethod("apply").appliedTo(arg.term)
        }

        val resultCases = cases.map { case CaseDef(p, g, rhs) =>
          val writerTerm = writerInfos.find(_._1 =:= rhs.tpe.widen).get._2
          CaseDef(p, g, writerTerm.selectWriteMethod.appliedTo(jsonName.asTerm, rhs, tokenWriter.asTerm))
        }

        Match(valueTerm, resultCases).asExprOf[Unit]
    }

    Expr.block(fieldExprs, '{ () })
  }

  private def allocateWriters(writerFields: List[WriterField]): List[(TypeRepr, Term)] = {
    val fieldInfos: List[TypeRepr] = writerFields.flatMap {
      case SimpleWriterField(name, _, tpe, _) => List(tpe)
      case PartialExtractedField(name, _, _, cases) =>
        cases.map { case CaseDef(_, _, rhs) =>
          rhs.tpe.widen
        }
    }

    fieldInfos.foldLeft(List[(TypeRepr, Term)]()) {
      case (info, fieldTpe) if !info.exists(_._1 =:= fieldTpe) =>
        val term =
          if (fieldTpe =:= TypeRepr.of[Nothing]) '{ EmptyWriters.emptyWriter[Nothing] }.asTerm
          else fieldTpe.searchJsonWriter

        (fieldTpe, term) :: info

      case (res, _) => res
    }
  }

  private def applyFieldStyle(fieldStyle: Option[FieldStyle]): List[WriterField] => List[WriterField] = writerFields =>
    fieldStyle.fold(writerFields) { style =>
      writerFields.map {
        case field: SimpleWriterField => field.copy(jsonName = Expr(style.applyStyle(field.name)))
        case field                    => field
      }
    }

  private def applyDescriptionOperations(
      valueTerm: Term,
      operations: Seq[WriterMacroOperation]
  ): List[WriterField] => List[WriterField] = writerFields => {
    def mapField(fields: List[WriterField], name: String)(f: SimpleWriterField => WriterField): List[WriterField] = {
      fields.map {
        case field: SimpleWriterField if field.name == name => f(field)
        case field                                          => field
      }
    }

    operations.foldLeft(writerFields) { case (fields, operation) =>
      operation match {
        case WriterMacroOperation.Remove(_, field) =>
          fields.filterNot(_.name == field)

        case WriterMacroOperation.Update(_, field, name, fun, from, to) =>
          mapField(fields, field)(f =>
            SimpleWriterField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              tpe = to,
              extractor = FunctionExtractor(
                arg = InlineExtract(valueTerm.selectField(field)),
                from = from,
                to = to,
                body = fun
              )
            )
          )

        case WriterMacroOperation.UpdateFromRoot(tpe, field, name, fun, to) =>
          mapField(fields, field)(f =>
            SimpleWriterField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              tpe = to,
              extractor = FunctionExtractor(
                arg = InlineExtract(valueTerm),
                from = tpe,
                to = to,
                body = fun
              )
            )
          )

        case WriterMacroOperation.UpdatePartial(_, field, name, fun, _) =>
          mapField(fields, field)(f =>
            PartialExtractedField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              argExtractor = InlineExtract(valueTerm.selectField(field)),
              cases = fun match {
                case Lambda(_, Match(_, cases)) => cases
              }
            )
          )

        case WriterMacroOperation.UpdatePartialFromRoot(_, field, name, fun) =>
          mapField(fields, field)(f =>
            PartialExtractedField(
              name = field,
              jsonName = name.getOrElse(f.jsonName),
              argExtractor = InlineExtract(valueTerm),
              cases = fun match {
                case Lambda(_, Match(_, cases)) => cases
              }
            )
          )

        case WriterMacroOperation.Add(tpe, field, fun, to) =>
          fields ::: List(
            SimpleWriterField(
              name = "__---nope---__",
              jsonName = field,
              tpe = to,
              extractor = FunctionExtractor(
                arg = InlineExtract(valueTerm),
                from = tpe,
                to = to,
                body = fun
              )
            )
          )
      }
    }
  }

  private def makeFields[T: Type](valueTerm: Term): List[WriterField] = {
    val tpr = TypeRepr.of[T]
    tpr.typeSymbol.caseFields.map { fieldSym =>
      val fieldName = fieldSym.name
      SimpleWriterField(
        name = fieldName,
        jsonName = Expr(fieldName),
        tpe = tpr.memberType(fieldSym),
        extractor = InlineExtract(valueTerm.selectField(fieldName))
      )
    }
  }

  // ------------------------- SEALED (TRAIT | ABSTRACT CLASS) OR ENUM -------------------------- //
  def deriveSealedClassWriteValues[T: Type](value: Expr[T], tokenWriter: Expr[TokenWriter])(
      cfg: Expr[WriterDerivationConfig]
  ): Expr[Unit] = {
    val parentTerm = value.asTerm
    val parentTpr = TypeRepr.of[T]
    val parentSym = parentTpr.typeSymbol

    val children = collectDistinctSubtypes(parentTpr).sortBy(_.typeSymbol.fullName)

    val (_, discriminator) = evalWriterConfig(cfg)

    if (children.isEmpty) report.errorAndAbort(s"${parentSym.name} has no known direct subclass")
    else {
      val cases = children.map { childTpr =>
        val childSym = childTpr.typeSymbol
        val childTpt = childTpr.asType match { case '[t] => TypeTree.of[t] }
        val childWriteValuesMethod = childTpr.getWriteValuesMethod
        val childBind = Symbol.newBind(Symbol.spliceOwner, "c", Flags.EmptyFlags, childTpr)
        val writeValuesExpr = childWriteValuesMethod.appliedTo(Ref(childBind), tokenWriter.asTerm)

        val discriminatorExpr = discriminator.fold(Literal(UnitConstant())) { discriminator =>
          TypeRepr
            .of[String]
            .getWriteMethod
            .appliedTo(Expr(discriminator).asTerm, Expr(childSym.name.stripSuffix("$").trim).asTerm, tokenWriter.asTerm)
        }
        val rhs = Block(List(writeValuesExpr), discriminatorExpr)
        CaseDef(Bind(childBind, Typed(Ref(childBind), childTpt)), None, rhs)
      }

      Match(parentTerm, cases).asExprOf[Unit]
    }
  }

  private def collectDistinctSubtypes(baseTpe: TypeRepr): List[TypeRepr] = {
    def collectSubclasses(parent: Symbol): List[Symbol] = {
      parent.children.flatMap { child =>
        if (child.flags.is(Flags.Sealed) && (child.flags.is(Flags.Trait) || child.flags.is(Flags.Abstract)))
          collectSubclasses(child)
        else
          List(child)
      }
    }

    val baseSym = baseTpe.typeSymbol
    val baseArgs = baseTpe.getTypeArgs
    val children = collectSubclasses(baseSym)

    val tpes = children.map { childSym =>
      val childTpe =
        if (childSym.isType) TypeIdent(childSym).tpe else ValDef(childSym, None).tpt.tpe // TODO: Add Scala 3 Enum

      def substituteArgs: List[TypeRepr] = {
        val subst = childTpe.baseType(baseSym).getTypeArgs

        childSym.typeMembers.map { param =>
          val paramTpe = TypeIdent(param).tpe
          val index = subst.indexWhere(_ =:= paramTpe)
          if (index != -1) baseArgs(index)
          else
            report.errorAndAbort(s"$childSym contains additional type parameter that can't be derived in compile time")
        }
      }

      childTpe.appliedTo(substituteArgs)
    }

    tpes.foldLeft(List.empty[TypeRepr]) { case (acc, t) =>
      if (!acc.exists(_ =:= t)) t :: acc
      else acc
    }
  }

  private sealed trait Extractor
  private case class InlineExtract(term: Term) extends Extractor
  private case class FunctionExtractor(arg: InlineExtract, from: TypeRepr, to: TypeRepr, body: Term) extends Extractor

  private sealed trait WriterField {
    def name: String
  }
  private case class SimpleWriterField(name: String, jsonName: Expr[String], tpe: TypeRepr, extractor: Extractor)
      extends WriterField
  private case class PartialExtractedField(
      name: String,
      jsonName: Expr[String],
      argExtractor: Extractor,
      cases: List[CaseDef]
  ) extends WriterField
}
