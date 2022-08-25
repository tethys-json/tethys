package tethys.derivation.impl.derivation

import scala.collection.mutable.{HashMap => MutableMap}
import scala.quoted.*

import tethys.JsonReader
import tethys.derivation.builder.ReaderDerivationConfig
import tethys.derivation.impl.FieldStyle
import tethys.derivation.impl.builder.ReaderBuilderCommons
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.readers.{FieldName, JsonReaderDefaultValue, ReaderError}

trait ReaderDerivation extends ReaderBuilderCommons {
  import context.reflect.*

  def deriveCaseClassReadWithDescription[T: Type](
      description: MacroReaderDescription
  ): Expr[JsonReader[T]] = {
    val (fieldStyle, isStrict) = evalReaderConfig(description.config)

    val tpt = TypeTree.of[T]
    val tpr = tpt.tpe

    val classFields = tpr.typeSymbol.caseFields

    val notSortedReaderFields = applyFieldStyle(fieldStyle)
      .andThen(applyDescriptionOperations(description.operations))
      .apply(
        classFields.map { fieldSym =>
          val fieldName = fieldSym.name
          SimpleField(
            name = fieldName,
            tpe = tpr.memberType(fieldSym),
            jsonName = fieldName
          )
        }
      )

    val fieldsToReadFirstFromJson: List[String] = notSortedReaderFields.flatMap {
      case SimpleField(name, _, _) => List(name)
      case ExtractedField(_, _, args, _) =>
        args.collect { case FunctionArgument(Field.RawField(jsonName, _)) => jsonName }
      case FromExtractedReader(_, _, _, args, _) =>
        args.collect { case FunctionArgument(Field.RawField(jsonName, _)) => jsonName }
    }.distinct
    val sortedReaderFields = sortFieldsByProcessing(fieldsToReadFirstFromJson, notSortedReaderFields)

    val typeReadersInfos = allocateTypeReadersInfos(notSortedReaderFields)
    val defaultValuesExpr = allocateDefaultValuesFromDefinition[T]
    val possiblyNotInitializedExpr = allocateDefaultValuesForNotInitialized(sortedReaderFields)
    val (readersExpr, fieldsWithoutReadersExpr) = allocateReadersExpr(sortedReaderFields, typeReadersInfos)

    '{
      new JsonReader[T] {
        private[this] implicit def thisWriter: JsonReader[T] = this

        override def read(it: TokenIterator)(implicit fieldName: FieldName): T = {
          if (!it.currentToken().isObjectStart)
            ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)
          else {
            it.nextToken()
            val readFields = new MutableMap[String, MutableMap[String, Any]].empty.withDefaultValue(MutableMap.empty)
            val notComputedFields = new MutableMap[String, TokenIterator].empty
            val resultFields = new MutableMap[String, Any].empty

            val readers: Map[String, List[(String, String, JsonReader[?])]] = $readersExpr // jsonName -> (name, tpeName, reader)
            val fieldsWithoutReaders: Set[String] = $fieldsWithoutReadersExpr // jsonNames

            while (!it.currentToken().isObjectEnd) {
              val jsonName = it.fieldName()
              it.nextToken()
              val currentIt = it.collectExpression()

              readers
                .get(jsonName)
                .fold(
                  if (fieldsWithoutReaders.contains(jsonName))
                    notComputedFields.update(jsonName, currentIt)
                )(_.foreach { case (name, tpeName, reader) =>
                  val value: Any = reader.read(currentIt.copy())(fieldName.appendFieldName(jsonName))
                  readFields.updateWith(name) {
                    case None         => Some(MutableMap(tpeName -> value))
                    case Some(values) => Some(values.addOne(tpeName, value))
                  }
                })
            }
            it.nextToken()

            $possiblyNotInitializedExpr.foreach { case (name, tpeName, defaultValue) =>
              readFields.getOrElseUpdate(name, MutableMap(tpeName -> defaultValue))
            }

            val fieldsNotReadFirstInJson: Set[String] =
              Set.from(${ Varargs(fieldsToReadFirstFromJson.map(Expr(_))) }) -- readFields.keySet
            if (fieldsNotReadFirstInJson.nonEmpty)
              ReaderError.wrongJson("Can not extract fields from json: " + fieldsNotReadFirstInJson.mkString(", "))

            ${
              val readFieldsTerm = 'readFields.asTerm
              val resultFieldsTerm = 'resultFields.asTerm

              val res = sortedReaderFields.map {
                case SimpleField(name, tpe, _) =>
                  val nameTerm = Expr(name).asTerm
                  val tpeNameTerm = Expr(tpe.getDealiasFullName).asTerm
                  val resultField = readFieldsTerm
                    .selectFirstMethod("apply")
                    .appliedTo(nameTerm)
                    .selectFirstMethod("apply")
                    .appliedTo(tpeNameTerm)
                    .selectFirstMethod("asInstanceOf")
                    .appliedToType(tpe)
                  resultFieldsTerm
                    .selectFirstMethod("update")
                    .appliedTo(nameTerm, resultField)
                    .asExpr

                case ExtractedField(name, tpe, args, body) =>
                  val nameTerm = Expr(name).asTerm
                  val tpeNameTerm = Expr(tpe.getDealiasFullName).asTerm
                  val extractedArgs: List[Term] = args.map( arg =>
                    readFieldsTerm
                      .selectFirstMethod("apply")
                      .appliedTo(Expr(arg.field.name).asTerm)
                      .selectFirstMethod("apply")
                      .appliedTo(Expr(arg.field.tpe.getDealiasFullName).asTerm)
                      .selectFirstMethod("asInstanceOf")
                      .appliedToType(arg.field.tpe)
                  )
                  val extractedFieldTerm = body.selectFirstMethod("apply").appliedToArgs(extractedArgs)
                  val addToReadFieldsExpr = readFieldsTerm
                    .selectFirstMethod("apply")
                    .appliedTo(nameTerm)
                    .selectFirstMethod("update")
                    .appliedTo(tpeNameTerm, extractedFieldTerm)
                    .asExpr
                  val addToResultFieldsExpr = resultFieldsTerm
                    .selectFirstMethod("update")
                    .appliedTo(nameTerm, extractedFieldTerm)
                    .asExpr
                  Expr.block(List(addToReadFieldsExpr), addToResultFieldsExpr)

                case FromExtractedReader(name, tpe, jsonName, args, body) =>
                  val extractedArgs: List[Term] = args.map(arg =>
                    readFieldsTerm
                      .selectFirstMethod("apply")
                      .appliedTo(Expr(arg.field.name).asTerm)
                      .selectFirstMethod("apply")
                      .appliedTo(Expr(arg.field.tpe.getDealiasFullName).asTerm)
                      .selectFirstMethod("asInstanceOf")
                      .appliedToType(arg.field.tpe)
                  )
                  val nameExpr = Expr(name)
                  val jsonNameExpr = Expr(jsonName)
                  val tpeNameExpr = Expr(tpe.getDealiasFullName)
                  val extractedReaderExpr =
                    body.selectFirstMethod("apply").appliedToArgs(extractedArgs).asExprOf[JsonReader[?]]
                  '{
                    notComputedFields
                      .get($jsonNameExpr)
                      .foreach { it =>
                        val value: Any = $extractedReaderExpr.read(it)(fieldName.appendFieldName($jsonNameExpr))
                        readFields.updateWith($nameExpr) {
                          case None => Some(MutableMap($tpeNameExpr -> value))
                          case Some(values) => Some(values.addOne($tpeNameExpr, value))
                        }
                        resultFields.update($nameExpr, value)
                      }
                  }
              }
              Expr.block(res, '{ () })
            }

            $defaultValuesExpr.foreach { case (name, defaultValue) =>
              resultFields.getOrElseUpdate(name, defaultValue)
            }

            val notReadAfterExtractingFiedls: Set[String] =
              Set.from(${ Varargs(classFields.map(field => Expr(field.name))) }) -- resultFields.keySet
            if (notReadAfterExtractingFiedls.nonEmpty)
              ReaderError.wrongJson(
                "Can not extract fields: " + notReadAfterExtractingFiedls.mkString(", ")
              )

            ${
              val paramsInfo = classFields.map(param => (param.name, tpr.memberType(param)))

              val fields: List[Term] = paramsInfo.map { case (paramName, paramType) =>
                'resultFields.asTerm
                  .selectFirstMethod("apply")
                  .appliedTo(Expr(paramName).asTerm)
                  .selectFirstMethod("asInstanceOf")
                  .appliedToType(paramType)
              }

              New(tpt)
                .select(tpr.typeSymbol.primaryConstructor)
                .appliedToArgs(fields)
                .asExprOf[T]
            }
          }
        }
      }
    }
  }

  private def sortFieldsByProcessing(
      fieldsToReadFirstFromJson: List[String],
      readerFields: List[ReaderField]
  ): List[ReaderField] = {
    def go(notInQueue: List[ReaderField], queue: List[String]): List[String] = {
      notInQueue match {
        case Nil => queue
        case notInQueueFields =>
          val extractedFieldsName = queue.toSet
          val (ready, notReady) = notInQueueFields.partition {
            case extField: ExtractedField =>
              val args = extField.args.map(_.field.name).toSet
              args.isEmpty || args.subsetOf(extractedFieldsName)
            case fromExtReader: FromExtractedReader =>
              val args = fromExtReader.args.map(_.field.name).toSet
              args.isEmpty || args.subsetOf(extractedFieldsName)
            case _: SimpleField => true // impossible case
          }
          if (notReady != notInQueue)
            go(notReady, queue ::: ready.map(_.name))
          else
            report.errorAndAbort(
              s"Check fields in functions to extract: ${notReady.map(_.name).mkString(", ")}. There is recursive extracting or missing field possibly."
            )
      }
    }

    val startQueue = fieldsToReadFirstFromJson
    val notInQueue = readerFields.filterNot(field => startQueue.contains(field.name))
    val fieldsQueueToProcessing = go(notInQueue, startQueue)

    fieldsQueueToProcessing.flatMap(fieldName => readerFields.find(_.name == fieldName))
  }

  private def applyFieldStyle(fieldStyle: Option[FieldStyle]): List[SimpleField] => List[SimpleField] = readerFields =>
    fieldStyle.fold(readerFields)(style =>
      readerFields.map(field => field.copy(jsonName = style.applyStyle(field.jsonName)))
    )

  private def allocateDefaultValuesForNotInitialized(readerFields: List[ReaderField]): Expr[List[(String, String, Any)]] = {
    val fieldInfos = readerFields.flatMap {
      case f: SimpleField         => List(f.name -> f.tpe)
      case f: ExtractedField      => f.args.map(arg => arg.field.name -> arg.field.tpe)
      case f: FromExtractedReader => f.args.map(arg => arg.field.name -> arg.field.tpe)
    }

    val res = fieldInfos.flatMap { case (name, tpe) =>
      tpe.asType match {
        case '[jrdvTpe] =>
          tpe.searchJsonReaderDefaultValue.asExprOf[JsonReaderDefaultValue[jrdvTpe]] match {
            case '{ JsonReaderDefaultValue.noDefaultValue[jrdvTpe] } => None
            case jrdv =>
              Some(Expr.ofTuple((Expr(name), Expr(tpe.getDealiasFullName), jrdv.asTerm.selectFirstMethod("defaultValue").asExprOf[Any])))
          }
      }
    }

    Expr.ofList(res)
  }

  private def allocateReadersExpr(
      readerFields: List[ReaderField],
      readers: List[(TypeRepr, Term)]
  ): (Expr[Map[String, List[(String, String, JsonReader[?])]]], Expr[Set[String]]) = {
    case class FieldDef(name: String, jsonName: String, tpeFullName: String, reader: Term)

    def findReader(tpe: TypeRepr): Term = readers.find(_._1 =:= tpe.widen).get._2

    val fieldDefs: List[FieldDef] = readerFields.flatMap {
      case f: SimpleField =>
        List(FieldDef(f.name, f.jsonName, f.tpe.getDealiasFullName, findReader(f.tpe)))
      case f: ExtractedField =>
        f.args.collect { case FunctionArgument(Field.RawField(jsonName, tpe)) =>
          FieldDef(jsonName, jsonName, tpe.getDealiasFullName, findReader(tpe))
        }
      case f: FromExtractedReader =>
        f.args.collect { case FunctionArgument(Field.RawField(jsonName, tpe)) =>
          FieldDef(jsonName, jsonName, tpe.getDealiasFullName, findReader(tpe))
        }
    }

    val fieldsWithoutReaders = readerFields
      .filterNot(field => fieldDefs.exists(_.jsonName == field.jsonName))
      .map(field => Expr(field.jsonName))

    val groupedDefs = fieldDefs.distinct.groupBy(_.jsonName).toList

    val res = groupedDefs.map { case (jsonName, defs) =>
      val readersInfoExpr: Expr[List[(String, String, JsonReader[?])]] = Expr.ofList(
        defs.map { fieldDef =>
          Expr.ofTuple((Expr(fieldDef.name), Expr(fieldDef.tpeFullName), fieldDef.reader.asExprOf[JsonReader[?]]))
        }
      )
      Expr.ofTuple(Expr(jsonName) -> readersInfoExpr)
    }

    val readersExpr = '{ Map(${ Varargs(res) }: _*) }
    val fieldsWithoutReadersExpr = '{ Set(${ Varargs(fieldsWithoutReaders) }: _*) }
    (readersExpr, fieldsWithoutReadersExpr)
  }

  private def allocateDefaultValuesFromDefinition[T: Type]: Expr[Map[String, Any]] = {
    val tpe = TypeRepr.of[T]

    val res = tpe.typeSymbol.caseFields.flatMap {
      case sym if sym.flags.is(Flags.HasDefault) =>
        val comp = sym.owner.companionClass
        val mod = Ref(sym.owner.companionModule)
        val indexOfDefaultValueMethod = sym.owner.caseFields.indexOf(sym) + 1

        val defaultValueMethodSym =
          comp
            .declaredMethod(s"$$lessinit$$greater$$default$$$indexOfDefaultValueMethod") // $<init$>$default$1
            .headOption
            .getOrElse(
              report.errorAndAbort(
                s"Error while extracting default value for field '${sym.name}'. Default value method in companion class doesn't exist. Check default value for this parameter."
              )
            )

        val defaultValueTerm = mod.select(defaultValueMethodSym)
        Some(Expr.ofTuple(Expr(sym.name) -> defaultValueTerm.asExprOf[Any]))
      case _ => None
    }

    '{ Map(${ Varargs(res) }: _*) }
  }

  private def allocateTypeReadersInfos(readerFields: List[ReaderField]): List[(TypeRepr, Term)] = {
    val jsonTypes = readerFields.flatMap {
      case f: SimpleField         => List(f.tpe)
      case f: ExtractedField      => f.args.map(_.field.tpe)
      case f: FromExtractedReader => f.args.map(_.field.tpe)
    }

    jsonTypes.foldLeft(List[(TypeRepr, Term)]()) {
      case (acc, tpe) if !acc.exists(_._1 =:= tpe) =>
        (tpe, tpe.searchJsonReader) :: acc
      case (res, _) => res
    }
  }

  private def applyDescriptionOperations(
      operations: Seq[ReaderMacroOperation]
  ): List[ReaderField] => List[ReaderField] = readerFields => {
    def mapField(fields: List[ReaderField], name: String)(f: SimpleField => ReaderField): List[ReaderField] = {
      fields.map {
        case field: SimpleField if field.name == name => f(field)
        case field                                    => field
      }
    }

    def buildArgument(field: Field, readerFields: List[ReaderField]): FunctionArgument = {
      field match {
        case Field.ClassField(name, _) =>
          readerFields.collectFirst {
            case f: SimpleField if f.name == name         => FunctionArgument(field)
            case f: ExtractedField if f.name == name      => FunctionArgument(field)
            case f: FromExtractedReader if f.name == name => FunctionArgument(field)
          }.head
        case Field.RawField(name, tpe) =>
          val possibleArg = readerFields.flatMap {
            case f: SimpleField if f.jsonName == name && f.tpe =:= tpe =>
              List(FunctionArgument(field))
            case f: ExtractedField =>
              f.args.collectFirst {
                case arg @ FunctionArgument(rf: Field.RawField) if rf.name == name && rf.tpe =:= tpe =>
                  arg
              }
            case f: FromExtractedReader =>
              f.args.collectFirst {
                case arg @ FunctionArgument(rf: Field.RawField) if rf.name == name && rf.tpe =:= tpe =>
                  arg
              }
            case _ => List.empty[FunctionArgument]
          }

          possibleArg.headOption.getOrElse(FunctionArgument(field = Field.RawField(name, tpe)))
      }
    }

    operations.foldLeft(readerFields) { case (fields, operation) =>
      operation match {
        case ReaderMacroOperation.ExtractFieldAs(field, tpe, as, fun) =>
          mapField(fields, field)(f =>
            ExtractedField(
              name = field,
              tpe = tpe,
              args = List(FunctionArgument(field = Field.RawField(f.jsonName, as))),
              body = fun
            )
          )
        case ReaderMacroOperation.ExtractFieldValue(field, from, fun) =>
          mapField(fields, field)(f =>
            ExtractedField(
              name = field,
              tpe = f.tpe,
              args = from.toList.map(buildArgument(_, fields)),
              body = fun
            )
          )
        case ReaderMacroOperation.ExtractFieldReader(field, from, fun) =>
          mapField(fields, field)(f =>
            FromExtractedReader(
              name = field,
              tpe = f.tpe,
              jsonName = f.jsonName,
              args = from.toList.map(buildArgument(_, fields)),
              body = fun
            )
          )
      }
    }
  }

  private sealed trait ReaderField {
    val name: String
    val jsonName: String
  }
  private case class SimpleField(name: String, tpe: TypeRepr, jsonName: String) extends ReaderField

  private case class ExtractedField(name: String, tpe: TypeRepr, args: List[FunctionArgument], body: Term)
      extends ReaderField {
    val jsonName: String = name
  }

  private case class FromExtractedReader(
      name: String,
      tpe: TypeRepr,
      jsonName: String,
      args: List[FunctionArgument],
      body: Term
  ) extends ReaderField

  private case class FunctionArgument(field: Field)
}
