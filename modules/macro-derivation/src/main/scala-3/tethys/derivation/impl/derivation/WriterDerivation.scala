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
  def deriveCaseClassWriter[T: Type](
      description: MacroWriteDescription
  ): Expr[JsonObjectWriter[T]] = {
    val (fieldStyle, _) = evalWriterConfig(description.config)

    '{
      new JsonObjectWriter[T] {
        private[this] implicit def thisWriter: JsonWriter[T] =
          $searchWriterWithSameType.getOrElse(this)

        override def writeValues(value: T, tokenWriter: TokenWriter): Unit = ${
          val valueTerm = 'value.asTerm
          val tokenWriterTerm = 'tokenWriter.asTerm
          val writerFields = applyFieldStyle(fieldStyle)
            .andThen(applyDescriptionOperations(valueTerm, description.operations))
            .apply(makeFields[T](valueTerm))

          val writerInfos = allocateWriters(writerFields)

          val fieldWriteTerms = writerFields.map {
            case SimpleWriterField(_, jsonName, fieldTpe, extractor) =>
              val valueTerm = extractor match {
                case InlineExtract(term) =>
                  term
                case FunctionExtractor(arg, _, _, func) =>
                  func.selectFirstMethod("apply").appliedTo(arg.term)
              }

              val writerTerm = writerInfos.find(_._1 =:= fieldTpe.widen).get._2
              writerTerm.selectWrite3Method.appliedTo(jsonName.asTerm, valueTerm, tokenWriterTerm)

            case PartialExtractedField(_, jsonName, argExtractor, cases) =>
              val valueTerm = argExtractor match {
                case InlineExtract(term) =>
                  term
                case FunctionExtractor(arg, _, _, func) =>
                  func.selectFirstMethod("apply").appliedTo(arg.term)
              }

              val resultCases = cases.map { case CaseDef(p, g, rhs) =>
                val writerTerm = writerInfos.find(_._1 =:= rhs.tpe.widen).get._2
                CaseDef(p, g, writerTerm.selectWrite3Method.appliedTo(jsonName.asTerm, rhs, tokenWriterTerm))
              }

              Match(valueTerm, resultCases)
          }

          Block(fieldWriteTerms, '{ () }.asTerm).asExprOf[Unit]
        }
      }
    }
  }

  private def allocateWriters(writerFields: List[WriterField]): List[(TypeRepr, Term)] = {
    val fieldInfos: List[TypeRepr] = writerFields.flatMap {
      case SimpleWriterField(_, _, tpe, _)       => List(tpe)
      case PartialExtractedField(_, _, _, cases) => cases.map { case CaseDef(_, _, rhs) => rhs.tpe.widen }
    }

    fieldInfos.foldLeft(List[(TypeRepr, Term)]()) {
      case (info, fieldTpe) if !info.exists(_._1 =:= fieldTpe) =>
        val term =
          if (fieldTpe =:= TypeRepr.of[Nothing]) '{ EmptyWriters.emptyWriter[Nothing] }.asTerm
          else fieldTpe.searchInlineJsonWriter

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

  // -------------------------- SEALED (TRAIT | ABSTRACT CLASS) OR ENUM ------------------------------- //
  def deriveSealedClassWriter[T: Type](
      cfg: Expr[WriterDerivationConfig]
  ): Expr[JsonObjectWriter[T]] = {
    val parentTpr = TypeRepr.of[T]
    val (_, discriminator) = evalWriterConfig(cfg)
    val children = collectDistinctSubtypes(parentTpr).sortBy(_.typeSymbol.fullName)

    if (children.isEmpty) report.errorAndAbort(s"${parentTpr.show} has no known direct subclasses")

    '{
      new JsonObjectWriter[T] { self =>
        private[this] implicit def thisWriter: JsonWriter[T] =
          $searchWriterWithSameType.getOrElse(this)

        override def write(value: T, tokenWriter: TokenWriter): Unit = ${
          val cases = children.map {
            case typeChildTpr if typeChildTpr.termSymbol.isNoSymbol =>
              val typeChildTpt = typeChildTpr.asType match { case '[t] => TypeTree.of[t] }
              val typeChildBind = Symbol.newBind(Symbol.spliceOwner, "c", Flags.EmptyFlags, typeChildTpr)
              val rhs = '{
                tokenWriter.writeObjectStart()
                writeValues(value, tokenWriter)
                tokenWriter.writeObjectEnd()
                ()
              }.asTerm
              CaseDef(Bind(typeChildBind, Typed(Ref(typeChildBind), typeChildTpt)), None, rhs)
            case termChildTpr =>
              val termChildSym = termChildTpr.termSymbol
              val termChildRef = Ref(termChildSym)
              val rhs = '{ writeValues(value, tokenWriter) }.asTerm
              CaseDef(termChildRef, None, rhs)
          }

          Match('value.asTerm, cases).asExprOf[Unit]
        }

        override def writeValues(value: T, tokenWriter: TokenWriter): Unit = ${
          val valueTerm = 'value.asTerm
          val tokenWriterTerm = 'tokenWriter.asTerm

          val cases = children.map {
            case typeChildTpr if typeChildTpr.termSymbol.isNoSymbol =>
              val typeChildTpt = typeChildTpr.asType match { case '[t] => TypeTree.of[t] }
              val typeChildWriteValuesMethod = typeChildTpr.searchInlineJsonObjectWriter.selectWriteValuesMethod
              val typeChildBind = Symbol.newBind(Symbol.spliceOwner, "c", Flags.EmptyFlags, typeChildTpr)
              val writeValuesTerm = typeChildWriteValuesMethod.appliedTo(Ref(typeChildBind), tokenWriterTerm)
              val discriminatorTerm = discriminator.fold(Literal(UnitConstant())) { discriminator =>
                TypeRepr
                  .of[String]
                  .getWrite3Method
                  .appliedTo(Expr(discriminator).asTerm, Expr(typeChildTpr.typeSymbol.name).asTerm, tokenWriterTerm)
              }
              val rhs = Block(List(writeValuesTerm), discriminatorTerm)
              CaseDef(Bind(typeChildBind, Typed(Ref(typeChildBind), typeChildTpt)), None, rhs)
            case termChildTpr =>
              val termChildSym = termChildTpr.termSymbol
              val termChildRef = Ref(termChildSym)
              val termChildNameTerm = Expr(termChildSym.name).asTerm
              val termChildWriter = termChildTpr.searchJsonObjectWriter
              val discriminatorTerm = discriminator.fold(Literal(UnitConstant())) { discriminator =>
                TypeRepr
                  .of[String]
                  .getWrite3Method
                  .appliedTo(Expr(discriminator).asTerm, termChildNameTerm, tokenWriterTerm)
              }
              val terms: List[Term] =
                if (termChildWriter.underlying.symbol.flags.is(Flags.Macro))
                  List(
                    termChildWriter.selectWriteValuesMethod.appliedTo(termChildRef, tokenWriterTerm),
                    discriminatorTerm
                  )
                else {
                  val writeObjectStartTerm = tokenWriterTerm.selectFirstMethod("writeObjectStart").appliedToNone
                  val writeValuesTerm = termChildWriter.selectWriteValuesMethod.appliedTo(termChildRef, tokenWriterTerm)
                  val writeObjectEndTerm = tokenWriterTerm.selectFirstMethod("writeObjectEnd").appliedToNone
                  List(writeObjectStartTerm, writeValuesTerm, discriminatorTerm, writeObjectEndTerm)
                }
              val rhs = Block(terms, '{ () }.asTerm)
              CaseDef(termChildRef, None, rhs)
          }

          Match(valueTerm, cases).asExprOf[Unit]
        }
      }
    }
  }

  // -------------------------------- TERM (case objects, simple enums) ------------------------------- //
  def deriveTermWriter[T: Type]: Expr[JsonObjectWriter[T]] = {
    val termTpr = TypeRepr.of[T]
    val termSym = termTpr.termSymbol
    val termRef = Ref(termSym)
    val termNameTerm = Expr(termSym.name).asTerm

    '{
      new JsonObjectWriter[T] {
        override def write(value: T, tokenWriter: TokenWriter): Unit =
          writeValues(value, tokenWriter)

        override def writeValues(value: T, tokenWriter: TokenWriter): Unit = ${
          val tokenWriterTerm = 'tokenWriter.asTerm
          TypeRepr.of[String].getWrite2Method.appliedTo(termNameTerm, tokenWriterTerm).asExprOf[Unit]
        }
      }
    }
  }

  private implicit def searchWriterWithSameType[T: Type]: Expr[Option[JsonWriter[T]]] =
    Expr.summon[JsonWriter[T]].map(w => '{ Some($w) }).getOrElse('{ None })

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
