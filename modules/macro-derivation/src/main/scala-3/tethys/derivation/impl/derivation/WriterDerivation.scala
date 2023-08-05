package tethys.derivation.impl.derivation

import scala.annotation.tailrec
import scala.compiletime.*
import scala.quoted.*
import tethys.commons.LowPriorityInstance
import tethys.commons.TokenNode.FieldNameNode
import tethys.derivation.builder.WriterDerivationConfig
import tethys.derivation.impl.builder.{WriterBuilderCommons, WriterBuilderUtils}
import tethys.derivation.impl.FieldStyle
import tethys.writers.tokens.{SimpleTokenWriter, TokenWriter}
import tethys.{JsonObjectWriter, JsonWriter}

trait WriterDerivation extends WriterBuilderCommons {
  import context.reflect.*

  // ---------------------------------- CASE CLASS ----------------------------------
  def deriveCaseClassWriter[T: Type](
      description: MacroWriteDescription
  ): Expr[JsonObjectWriter[T]] = {
    val (fieldStyle, _) = evalWriterConfig(description.config)
    val caseClassTpe: TypeRepr = TypeRepr.of[T]

    val defaultFieldsToTpesMap: Map[String, List[TypeRepr]] = Map.from {
      caseClassTpe.typeSymbol.caseFields.map(sym =>
        sym.name -> List(caseClassTpe.memberType(sym))
      )
    }
    val allTpes: List[TypeRepr] = collectAllNecessaryTypes(
      defaultFieldsToTpesMap,
      description.operations.toList
    )
    val (nonCaseClassTpes, caseClassTpes) =
      allTpes.partition(_.typeSymbol.caseFields.isEmpty)

    def collectClsDecls(clsSym: Symbol): List[Symbol] =
      createThisWriterSym(clsSym, caseClassTpe) +:
        allTpes.zipWithIndex.map { case (tpe, i) =>
          createInnerWriterSym(clsSym, tpe.wrappedTo[JsonWriter], i)
        } :+
        createWriteValuesMethodSym(
          clsSym,
          name = "writeValues",
          tpe = caseClassTpe
        )

    val clsSym: Symbol = createClsSym(caseClassTpe, collectClsDecls)

    val thisWriterValDef: ValDef = ValDef(
      symbol = clsSym.declaredField("thisWriter"),
      rhs = Some(This(clsSym))
    )

    val tpeTermPairs: List[(TypeRepr, Term)] = createNonCaseClassWriterTerms(
      nonCaseClassTpes
    ) ++ createCaseClassWriterTerms(caseClassTpes)
    val innerWriterDefs: List[ValDef] =
      createInnerWriterDefs(clsSym, tpeTermPairs)

    val writeValuesMethodDef: DefDef = createWriteValuesMethodDef[T](
      symbol = clsSym.declaredMethod("writeValues").head,
      fieldStyle,
      operations = description.operations,
      writerDefs = innerWriterDefs
    )

    val derivedWriterClsParents: List[TypeTree] =
      List(TypeTree.of[Object], TypeTree.of[JsonObjectWriter[T]])
    val clsBody: List[Statement] =
      thisWriterValDef +: innerWriterDefs :+ writeValuesMethodDef
    createDerivedWriterExpr[T](clsSym, derivedWriterClsParents, clsBody)
  }

  private def createInnerWriterDefs(
      clsSym: Symbol,
      tpeTermPairs: List[(TypeRepr, Term)]
  ): List[ValDef] =
    tpeTermPairs.flatMap { case (tpe, writerTerm) =>
      clsSym.declaredFields
        .find(tpe.memberType(_).widen <:< tpe)
        .fold(List.empty) { writerSym =>
          ValDef(symbol = writerSym, rhs = Some(writerTerm)) :: Nil
        }
    }

  private def createDerivedWriterExpr[T: Type](
      clsSym: Symbol,
      clsParents: List[Tree],
      clsBody: List[Statement]
  ): Expr[JsonObjectWriter[T]] = {
    val derivedWriterClassDef: ClassDef =
      ClassDef(cls = clsSym, parents = clsParents, body = clsBody)
    val derivedWriterInstance: Typed = Typed(
      expr = Apply(
        fun = Select(New(TypeIdent(clsSym)), clsSym.primaryConstructor),
        args = Nil
      ),
      tpt = TypeTree.of[JsonObjectWriter[T]]
    )

    Block(stats = List(derivedWriterClassDef), expr = derivedWriterInstance)
      .asExprOf[JsonObjectWriter[T]]
  }

  private def collectAllNecessaryTypes(
      defaultTpes: Map[String, List[TypeRepr]],
      operations: List[WriterMacroOperation]
  ): List[TypeRepr] = {
    def searchAdditionalTypes(tpe: TypeRepr): List[TypeRepr] = {
      @tailrec
      def loop(acc: List[TypeRepr], unchecked: List[TypeRepr]): List[TypeRepr] =
        unchecked match
          case head :: tail =>
            head match {
              case tpe: AndOrType =>
                loop(acc, tail ++ List(tpe.left, tpe.right))
              case _ =>
                loop(acc :+ head, tail)
            }
          case Nil => acc

      loop(acc = Nil, unchecked = tpe :: Nil)
    }

    val allFieldsToTpesMap: Map[String, List[TypeRepr]] =
      operations.foldLeft(defaultTpes) { case (fieldToTpesMap, operation) =>
        operation match
          case op: WriterMacroOperation.Remove =>
            fieldToTpesMap - op.field
          case op: WriterMacroOperation.Update =>
            fieldToTpesMap + (op.field -> searchAdditionalTypes(op.to))
          case op: WriterMacroOperation.UpdateFromRoot =>
            fieldToTpesMap + (op.field -> searchAdditionalTypes(op.to))
          case op: WriterMacroOperation.UpdatePartial =>
            fieldToTpesMap + (op.field -> searchAdditionalTypes(op.to))
          case op: WriterMacroOperation.UpdatePartialFromRoot =>
            fieldToTpesMap + (op.field -> searchAdditionalTypes(op.to))
          case op: WriterMacroOperation.Add =>
            fieldToTpesMap + (op.field.value.get -> searchAdditionalTypes(
              op.to
            ))
      }

    allFieldsToTpesMap.values.toList.flatten.distinctBy(_.show)
  }

  private def createNonCaseClassWriterTerms(
      tpes: List[TypeRepr]
  ): List[(TypeRepr, Term)] =
    tpes.foldLeft(List.empty) {
      case (rest, fieldTpe) if !rest.exists(_._1 <:< fieldTpe.widen) =>
        val writerTpe: TypeRepr = fieldTpe.wrappedTo[JsonWriter]
        val writerTerm: Term =
          fieldTpe.createWriterTerm(_.searchInlineJsonWriter)
        rest :+ (writerTpe, writerTerm)
      case (rest, _) => rest
    }

  private def createCaseClassWriterTerms(
      tpes: List[TypeRepr]
  ): List[(TypeRepr, Term)] =
    tpes.map {
      _.asType match {
        case '[t] =>
          def derivedWriter: Expr[JsonWriter[t]] =
            deriveCaseClassWriter[t](MacroWriteDescription.empty[t])
          val writerTerm: Term =
            Expr.summon[JsonWriter[t]].getOrElse(derivedWriter).asTerm
          TypeRepr.of[JsonWriter[t]] -> writerTerm
      }
    }

  private def createWriteValuesMethodDef[T: Type](
      symbol: Symbol,
      fieldStyle: Option[FieldStyle],
      operations: Seq[WriterMacroOperation],
      writerDefs: List[ValDef]
  ): DefDef =
    DefDef(
      symbol = symbol,
      rhsFn = params => {
        val term: Term = {
          val (valueTerm, tokenWriterTerm) = getValueAndTokenWriterTerms(params)

          val writerFields: List[WriterField] = applyFieldStyle(fieldStyle)
            .andThen(applyDescriptionOperations(valueTerm, operations))
            .apply(makeFields[T](valueTerm))
          val stats: List[Statement] =
            createWriterStatements(writerFields, writerDefs, tokenWriterTerm)

          Block(stats, expr = '{ () }.asTerm)
        }

        Some(term.changeOwner(symbol))
      }
    )

  private def applyFieldStyle(
      fieldStyle: Option[FieldStyle]
  ): List[WriterField] => List[WriterField] =
    writerFields =>
      fieldStyle.fold(writerFields) { style =>
        writerFields.map {
          case field: SimpleWriterField =>
            field.copy(jsonName = Expr(style.applyStyle(field.name)))
          case field => field
        }
      }

  private def applyDescriptionOperations(
      valueTerm: Term,
      operations: Seq[WriterMacroOperation]
  ): List[WriterField] => List[WriterField] = writerFields => {
    def mapField(fields: List[WriterField], name: String)(
        f: SimpleWriterField => WriterField
    ): List[WriterField] =
      fields.map {
        case field: SimpleWriterField if field.name == name => f(field)
        case field                                          => field
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

        case WriterMacroOperation.UpdatePartial(_, field, name, fun, _, _) =>
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

        case WriterMacroOperation.UpdatePartialFromRoot(
              _,
              field,
              name,
              fun,
              _
            ) =>
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
          fields :+ SimpleWriterField(
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

  private def createWriterStatements(
      writerFields: List[WriterField],
      writerDefs: List[ValDef],
      tokenWriterTerm: Term
  ): List[Statement] =
    writerFields.map {
      case SimpleWriterField(_, jsonName, fieldTpe, extractor) =>
        getWriterDefRef(writerDefs, fieldTpe).selectWrite3Method
          .appliedTo(jsonName.asTerm, getValueTerm(extractor), tokenWriterTerm)

      case PartialExtractedField(_, jsonName, argExtractor, cases) =>
        val newCases: List[CaseDef] = cases.map {
          case CaseDef(pattern, guard, rhs) =>
            val writerTerm: Term =
              rhs.tpe.widen.createWriterTerm(getWriterDefRef(writerDefs, _))
            val newRhs: Term = writerTerm.selectWrite3Method
              .appliedTo(jsonName.asTerm, rhs, tokenWriterTerm)
            CaseDef(pattern, guard, rhs = newRhs)
        }
        Match(selector = getValueTerm(argExtractor), cases = newCases)
    }

  private def getWriterDefRef(
      writerDefs: List[ValDef],
      fieldTpe: TypeRepr
  ): Ref = {
    val fieldWriterTpe: TypeRepr = fieldTpe.wrappedTo[JsonWriter]
    val reqWriter: ValDef =
      writerDefs.find(_.tpt.tpe <:< fieldWriterTpe).getOrElse {
        report.errorAndAbort(
          s"Writer for type ${fieldTpe.show} hasn't been found between writer definitions: ${writerDefs
              .map(_.show)}"
        )
      }
    Ref(reqWriter.symbol)
  }

  private def getValueTerm(extractor: Extractor): Term = extractor match {
    case InlineExtract(term) =>
      term
    case FunctionExtractor(arg, _, _, func) =>
      func.selectFirstMethod("apply").appliedTo(arg.term)
  }

  // -------------------- SEALED (TRAIT | ABSTRACT CLASS) OR ENUM -------------------
  def deriveSealedClassWriter[T: Type](
      cfg: Expr[WriterDerivationConfig]
  ): Expr[JsonObjectWriter[T]] = {
    val parentTpe: TypeRepr = TypeRepr.of[T]
    val isSameTypeWriterExists: Boolean = Expr.summon[JsonWriter[T]].nonEmpty

    val (_, discriminator) = evalWriterConfig(cfg)
    val childTpes: List[TypeRepr] =
      collectDistinctSubtypes(parentTpe).sortBy(_.typeSymbol.fullName)

    if (childTpes.isEmpty)
      report.errorAndAbort(s"${parentTpe.show} has no known direct subclasses")

    def collectClsDecls(clsSym: Symbol): List[Symbol] =
      (if (isSameTypeWriterExists) Nil
       else List(createThisWriterSym(clsSym, parentTpe))) :::
        childTpes.zipWithIndex.map { case (tpe, i) =>
          createInnerWriterSym(clsSym, tpe.wrappedTo[JsonObjectWriter], i)
        } ::: List(
          createWriteValuesMethodSym(clsSym, name = "write", tpe = parentTpe),
          createWriteValuesMethodSym(
            clsSym,
            name = "writeValues",
            tpe = parentTpe
          )
        )

    val clsSym: Symbol = createClsSym(parentTpe, collectClsDecls)

    val writeMethodDefSym: Symbol = clsSym.declaredMethod("write").head
    val writeValuesMethodDefSym: Symbol =
      clsSym.declaredMethod("writeValues").head

    lazy val thisWriterDef: ValDef = ValDef(
      symbol = clsSym.declaredField("thisWriter"),
      rhs = Some(This(clsSym))
    )

    val tpeTermPairs: List[(TypeRepr, Term)] = createWriterTpeTermPairs(
      childTpes
    )
    val innerWriterDefs: List[ValDef] =
      createInnerWriterDefs(clsSym, tpeTermPairs)

    val writeMethodDef: DefDef = createSealedWriteMethodDef[T](
      writeMethodDefSym,
      childTpes,
      writeValuesMethodDefSym
    )
    val writeValuesMethodDef: DefDef = createSealedWriteValuesMethodDef[T](
      writeValuesMethodDefSym,
      childTpes,
      innerWriterDefs,
      discriminator
    )

    val derivedWriterClsParents: List[TypeTree] =
      List(TypeTree.of[Object], TypeTree.of[JsonObjectWriter[T]])
    val clsBody: List[Statement] =
      (if (isSameTypeWriterExists) Nil else List(thisWriterDef)) :::
        innerWriterDefs ::: List(writeMethodDef, writeValuesMethodDef)

    createDerivedWriterExpr[T](clsSym, derivedWriterClsParents, clsBody)
  }

  private def createWriterTpeTermPairs(
      tpes: List[TypeRepr]
  ): List[(TypeRepr, Term)] =
    tpes.foldLeft(List.empty) {
      case (rest, fieldTpe) if !rest.exists(_._1 <:< fieldTpe.widen) =>
        val writerTpe: TypeRepr = fieldTpe.wrappedTo[JsonWriter]
        val writerTerm: Term =
          fieldTpe.createWriterTerm(_.searchInlineJsonObjectWriter)
        rest :+ (writerTpe, writerTerm)
      case (rest, _) => rest
    }

  private def createSealedWriteMethodDef[T: Type](
      sym: Symbol,
      childTpes: List[TypeRepr],
      writeValuesMethodDefSym: Symbol
  ): DefDef =
    DefDef(
      symbol = sym,
      rhsFn = params => {
        val term: Term = {
          val (valueTerm, tokenWriterTerm) = getValueAndTokenWriterTerms(params)

          val cases: List[CaseDef] = childTpes.map {
            case typeChildTpr if typeChildTpr.termSymbol.isNoSymbol =>
              val typeChildTpt: TypeTree = typeChildTpr.asType match {
                case '[t] => TypeTree.of[t]
              }
              val typeChildBind = Symbol.newBind(
                Symbol.spliceOwner,
                "c",
                Flags.EmptyFlags,
                typeChildTpr
              )
              val terms: List[Term] = {
                val writeObjectStartTerm = tokenWriterTerm
                  .selectFirstMethod("writeObjectStart")
                  .appliedToNone
                val writeValuesTerm = Ref(writeValuesMethodDefSym)
                  .appliedTo(valueTerm, tokenWriterTerm)
                val writeObjectEndTerm = tokenWriterTerm
                  .selectFirstMethod("writeObjectEnd")
                  .appliedToNone
                List(writeObjectStartTerm, writeValuesTerm, writeObjectEndTerm)
              }
              val rhs = Block(terms, '{ () }.asTerm)
              CaseDef(
                Bind(typeChildBind, Typed(Ref(typeChildBind), typeChildTpt)),
                None,
                rhs
              )
            case termChildTpr =>
              val termChildSym: Symbol = termChildTpr.termSymbol
              val termChildRef: Ref = Ref(termChildSym)
              val rhs: Term = Ref(writeValuesMethodDefSym)
                .appliedTo(valueTerm, tokenWriterTerm)
              CaseDef(termChildRef, None, rhs)
          }

          Match(valueTerm, cases)
        }

        Some(term.changeOwner(sym))
      }
    )

  private def createSealedWriteValuesMethodDef[T: Type](
      sym: Symbol,
      childTpes: List[TypeRepr],
      writerDefs: List[ValDef],
      discriminator: Option[String]
  ): DefDef =
    DefDef(
      symbol = sym,
      rhsFn = params => {
        val term: Term = {
          val (valueTerm, tokenWriterTerm) = getValueAndTokenWriterTerms(params)

          val cases: List[CaseDef] = childTpes.map {
            case typeChildTpr if typeChildTpr.termSymbol.isNoSymbol =>
              val typeChildTpt: TypeTree = typeChildTpr.asType match {
                case '[t] => TypeTree.of[t]
              }
              val termChildWriter: Ref =
                getWriterDefRef(writerDefs, typeChildTpr)
              val typeChildWriteValuesMethod =
                termChildWriter.selectWriteValuesMethod
              val typeChildBind = Symbol.newBind(
                Symbol.spliceOwner,
                "c",
                Flags.EmptyFlags,
                typeChildTpr
              )
              val writeValuesTerm = typeChildWriteValuesMethod.appliedTo(
                Ref(typeChildBind),
                tokenWriterTerm
              )
              val discriminatorTerm =
                discriminator.fold(Literal(UnitConstant())) { discriminator =>
                  TypeRepr
                    .of[String]
                    .getWrite3Method
                    .appliedTo(
                      Expr(discriminator).asTerm,
                      Expr(typeChildTpr.typeSymbol.name).asTerm,
                      tokenWriterTerm
                    )
                }
              val rhs = Block(List(writeValuesTerm), discriminatorTerm)
              CaseDef(
                Bind(typeChildBind, Typed(Ref(typeChildBind), typeChildTpt)),
                None,
                rhs
              )
            case termChildTpr =>
              val termChildSym = termChildTpr.termSymbol
              val termChildRef = Ref(termChildSym)
              val termChildNameTerm = Expr(termChildSym.name).asTerm
              val termChildWriter = getWriterDefRef(writerDefs, termChildTpr)
              val discriminatorTerm =
                discriminator.fold(Literal(UnitConstant())) { discriminator =>
                  TypeRepr
                    .of[String]
                    .getWrite3Method
                    .appliedTo(
                      Expr(discriminator).asTerm,
                      termChildNameTerm,
                      tokenWriterTerm
                    )
                }
              val writeObjectStartTerm = tokenWriterTerm
                .selectFirstMethod("writeObjectStart")
                .appliedToNone
              val writeObjectEndTerm = tokenWriterTerm
                .selectFirstMethod("writeObjectEnd")
                .appliedToNone
              val terms: List[Term] =
                if (termChildSym.flags.is(Flags.Enum)) {
                  if (discriminator.isEmpty)
                    List(
                      termChildWriter.selectWriteValuesMethod
                        .appliedTo(termChildRef, tokenWriterTerm)
                    )
                  else
                    List(
                      writeObjectStartTerm,
                      discriminatorTerm,
                      writeObjectEndTerm
                    )
                } else {
                  val writeValuesTerm = termChildWriter.selectWriteValuesMethod
                    .appliedTo(termChildRef, tokenWriterTerm)
                  List(
                    writeObjectStartTerm,
                    writeValuesTerm,
                    discriminatorTerm,
                    writeObjectEndTerm
                  )
                }
              val rhs = Block(terms, '{ () }.asTerm)
              CaseDef(termChildRef, None, rhs)
          }

          Match(valueTerm, cases)
        }

        Some(term.changeOwner(sym))
      }
    )

  // ----------------------- TERM (case objects, simple enums) ----------------------
  def deriveTermWriter[T: Type]: Expr[JsonObjectWriter[T]] = {
    val termNameTerm = Expr(TypeRepr.of[T].termSymbol.name).asTerm

    '{
      new JsonObjectWriter[T] {
        override def write(value: T, tokenWriter: TokenWriter): Unit =
          writeValues(value, tokenWriter)

        override def writeValues(value: T, tokenWriter: TokenWriter): Unit = ${
          val tokenWriterTerm = 'tokenWriter.asTerm
          TypeRepr
            .of[String]
            .getWrite2Method
            .appliedTo(termNameTerm, tokenWriterTerm)
            .asExprOf[Unit]
        }
      }
    }
  }

  // ------------------------------------ COMMON ------------------------------------
  private def createClsSym(
      tpe: TypeRepr,
      declsFn: Symbol => List[Symbol]
  ): Symbol =
    Symbol.newClass(
      parent = Symbol.spliceOwner,
      name = tpe.typeSymbol.name + "_DerivedWriter",
      parents = List(TypeRepr.of[Object], tpe.wrappedTo[JsonObjectWriter]),
      decls = declsFn,
      selfType = None
    )

  private def createThisWriterSym(parentSymbol: Symbol, tpe: TypeRepr): Symbol =
    Symbol.newVal(
      parent = parentSymbol,
      name = "thisWriter",
      tpe = tpe.wrappedTo[JsonObjectWriter],
      flags = Flags.Lazy | Flags.Implicit,
      privateWithin = Symbol.noSymbol
    )

  private def createInnerWriterSym(
      parentSymbol: Symbol,
      tpe: TypeRepr,
      idx: Int
  ): Symbol =
    Symbol.newVal(
      parent = parentSymbol,
      name = s"innerWriter_$idx",
      tpe = tpe,
      flags = Flags.Lazy,
      privateWithin = Symbol.noSymbol
    )

  private def createWriteValuesMethodSym(
      parentSymbol: Symbol,
      name: String,
      tpe: TypeRepr
  ): Symbol =
    Symbol.newMethod(
      parent = parentSymbol,
      name = name,
      tpe = MethodType(paramNames = List("value", "tokenWriter"))(
        paramInfosExp = _ => List(tpe, TypeRepr.of[TokenWriter]),
        resultTypeExp = _ => TypeRepr.of[Unit]
      ),
      flags = Flags.Override,
      privateWithin = Symbol.noSymbol
    )

  private def getValueAndTokenWriterTerms[T: Type](
      params: List[List[Tree]]
  ): (Term, Term) = {
    val value: Expr[T] = params.head.head.asExprOf[T]
    val tokenWriter: Expr[TokenWriter] = params.head(1).asExprOf[TokenWriter]

    value.asTerm -> tokenWriter.asTerm
  }

  private sealed trait Extractor
  private case class InlineExtract(term: Term) extends Extractor
  private case class FunctionExtractor(
      arg: InlineExtract,
      from: TypeRepr,
      to: TypeRepr,
      body: Term
  ) extends Extractor

  private sealed trait WriterField {
    def name: String
  }
  private case class SimpleWriterField(
      name: String,
      jsonName: Expr[String],
      tpe: TypeRepr,
      extractor: Extractor
  ) extends WriterField
  private case class PartialExtractedField(
      name: String,
      jsonName: Expr[String],
      argExtractor: Extractor,
      cases: List[CaseDef]
  ) extends WriterField
}
