package tethys.derivation

import tethys.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.{constValueTuple, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, FromExpr, Quotes, ToExpr, Type, Varargs}

class ConfigurationMacroUtils(using val quotes: Quotes):
  import quotes.reflect.*

  def prepareWriterProductFields[T: Type](
      config: Expr[WriterBuilder[T]]
  ): List[WriterField] =
    val macroConfig = parseWriterBuilderMacroConfig[T](config)
    val updates = macroConfig.update.map(it => it.name -> it).toMap
    val tpe = TypeRepr.of[T]
    tpe.typeSymbol.caseFields.zipWithIndex
      .filterNot((symbol, _) => macroConfig.delete(symbol.name))
      .collect { (symbol, idx) =>
        val name = macroConfig.fieldStyle.fold(symbol.name)(
          FieldStyle.applyStyle(symbol.name, _)
        )
        updates.get(symbol.name) match
          case Some(update) =>
            WriterField.Basic(
              symbol.name,
              name,
              update.fun.map(WriterField.Update(_, update.what)),
              update.to,
              Some(update.newName)
            )
          case None =>
            WriterField.Basic(
              symbol.name,
              name,
              None,
              tpe = tpe.memberType(symbol),
              newName = None
            )
      } ::: macroConfig.add

  private def parseWriterBuilderMacroConfig[T: Type](
      config: Expr[WriterBuilder[T]]
  ): WriterBuilderMacroConfig =
    val fields = TypeRepr.of[T].typeSymbol.caseFields.map(_.name)

    def exitFieldAlreadyUpdated(name: String) =
      report.errorAndAbort(
        s"Ambigious updates found. Update for field '$name' already configured"
      )

    @tailrec
    def loop(
      config: Expr[WriterBuilder[T]],
      acc: WriterBuilderMacroConfig = WriterBuilderMacroConfig(),
      updatedFields: Set[String] = Set.empty
    ): (WriterBuilderMacroConfig, Set[String]) =
      config match
        case '{
              WriterBuilder[T](using ${ mirror }: Mirror.ProductOf[T])
            } =>
          (acc, updatedFields)

        case '{
              WriterBuilder[T](using
                ${ mirror }: Mirror.ProductOf[T]
              )
            } =>
          (acc, updatedFields)

        case '{
              ($rest: WriterBuilder[T]).fieldStyle(${
                fieldStyle
              }: FieldStyle)
            } =>
          (acc.copy(fieldStyle = Some(fieldStyle.valueOrAbort)), updatedFields)

        case '{
              ($rest: WriterBuilder[T])
                .add($field: String)
                .apply[to]($lambda)
            } =>
          field.value
            .filter(fields.contains(_))
            .foreach(field =>
              report.errorAndAbort(s"Field '$field' already exists")
            )
          loop(
            config = rest,
            acc = acc.withField(
              WriterField.Added(
                name = s"added$$macro$$${fields.length + acc.add.length + 1}",
                update = Some(
                  WriterField
                    .Update(lambda.asTerm, WriterField.Update.What.Root)
                ),
                tpe = TypeRepr.of[to],
                label = field
              )
            ),
            updatedFields = field.value.fold(updatedFields)(updatedFields + _)
          )

        case '{
              ($rest: WriterBuilder[T]).remove(${
                SelectedField(field)
              }: T => Any)
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withDelete(field.name),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T]).rename[to](${
                SelectedField(field)
              })($rename: String)
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ Some($rename) },
                what = WriterField.Update.What.Field,
                fun = None,
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .update[from](${ SelectedField(field) })
                .apply[to]($updater)
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ None },
                what = WriterField.Update.What.Field,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .update[from](${ SelectedField(field) })
                .withRename($rename)
                .apply[to]($updater)
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ Some($rename) },
                what = WriterField.Update.What.Field,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .update(${ SelectedField(field) })
                .fromRoot[to](${ updater })
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ None },
                what = WriterField.Update.What.Root,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .update[from](${ SelectedField(field) })
                .withRename($rename)
                .fromRoot[to](${ updater })
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ Some($rename) },
                what = WriterField.Update.What.Root,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )
        case '{
              ($rest: WriterBuilder[T])
                .updatePartial[from](${ SelectedField(field) })
                .apply[to]($updater)
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ None },
                what = WriterField.Update.What.Field,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .updatePartial[from](${ SelectedField(field) })
                .withRename($rename)
                .apply[to]($updater)
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ Some($rename) },
                what = WriterField.Update.What.Field,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .updatePartial(${ SelectedField(field) })
                .fromRoot[to](${ updater })
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ None },
                what = WriterField.Update.What.Root,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case '{
              ($rest: WriterBuilder[T])
                .updatePartial[from](${ SelectedField(field) })
                .withRename($rename)
                .fromRoot[to](${ updater })
            } =>
          if updatedFields.contains(field.name) then
            exitFieldAlreadyUpdated(field.name)

          loop(
            config = rest,
            acc = acc.withUpdate(
              WriterFieldUpdate(
                name = field.name,
                newName = '{ Some($rename) },
                what = WriterField.Update.What.Root,
                fun = Some(updater.asTerm),
                to = TypeRepr.of[to]
              )
            ),
            updatedFields = updatedFields + field.name
          )

        case other =>
          other.asTerm match
            case Inlined(_, _, term) =>
              loop(term.asExprOf[WriterBuilder[T]])
            case _ =>
              report.errorAndAbort(
                s"Unknown tree. Config must be an inlined given.\nTree: ${other.asTerm
                    .show(using Printer.TreeStructure)}"
              )

    loop(traverseTree(config.asTerm).asExprOf[WriterBuilder[T]])._1

  end parseWriterBuilderMacroConfig

  def prepareReaderProductFields[T: Type](
      config: Expr[ReaderBuilder[T]]
  ): (List[ReaderField], IsStrict) =
    val macroConfig = parseReaderBuilderMacroConfig[T](config)
    val tpe = TypeRepr.of[T]
    val defaults = collectDefaults[T]
    val fields = tpe.typeSymbol.caseFields.zipWithIndex
      .map { case (symbol, idx) =>
        val default = defaults.get(idx).map(_.asExprOf[Any])
        macroConfig.extracted.get(symbol.name) match
          case Some(field) =>
            field.update(idx, default, macroConfig.fieldStyle)
          case None =>
            ReaderField
              .Basic(symbol.name, tpe.memberType(symbol), None)
              .update(idx, default, macroConfig.fieldStyle)
      }
    checkLoops(fields)
    (sortDependencies(fields), macroConfig.isStrict)

  private def sortDependencies(fields: List[ReaderField]): List[ReaderField] =
    val known = fields.map(_.name).toSet
    val (basic, allExtracted) = fields.partitionMap {
      case field: ReaderField.Basic     => Left(field)
      case field: ReaderField.Extracted => Right(field)
    }
    @scala.annotation.tailrec
    def loop(
        extracted: List[ReaderField.Extracted],
        previous: List[ReaderField],
        n: Int
    ): List[ReaderField] =
      if extracted.isEmpty || n == allExtracted.length + 1 then previous
      else
        val processed = previous.map(_.name).toSet
        val (dependentOnPrevious, moreDependencies) =
          extracted.partition(
            _.extractors.forall((name, _) => processed(name) || !known(name))
          )
        loop(moreDependencies, previous ::: dependentOnPrevious, n + 1)

    loop(allExtracted, basic, n = 0)

  private def checkLoops(fields: List[ReaderField]): Unit =
    val extractedFields = fields.collect { case field: ReaderField.Extracted =>
      field
    }

    @scala.annotation.tailrec
    def loop(dependencies: Map[String, Set[String]], n: Int): Unit =
      if n == dependencies.size then ()
      else
        val looped = dependencies.collect {
          case (name, extractors) if extractors.contains(name) => name
        }
        if looped.nonEmpty then
          report.errorAndAbort(
            s"Found loop in your configuration, fields ${looped.mkString("[", ", ", "]")}"
          )
        val nextDependencies = dependencies.map((name, deps) =>
          (
            name,
            deps.flatMap(name => dependencies.getOrElse(name, Set.empty) + name)
          )
        )
        loop(nextDependencies, n + 1)

    loop(
      extractedFields
        .map(field => (field.name, field.extractors.map(_._1).toSet))
        .toMap,
      0
    )
  end checkLoops

  private def collectDefaults[T: Type]: Map[Int, Term] =
    val tpe = TypeRepr.of[T]
    tpe.typeSymbol.primaryConstructor.paramSymss.flatten
      .filter(_.isValDef)
      .zipWithIndex
      .flatMap { (field, idx) =>
        val defaultMethodName = s"$$lessinit$$greater$$default$$${idx + 1}"
        tpe.typeSymbol.companionClass
          .declaredMethod(defaultMethodName)
          .headOption
          .map { defaultMethod =>
            val callDefault = {
              val base = Ident(tpe.typeSymbol.companionModule.termRef).select(
                defaultMethod
              )
              val tParams =
                defaultMethod.paramSymss.headOption.filter(_.forall(_.isType))
              tParams match
                case Some(tParams) => TypeApply(base, tParams.map(TypeTree.ref))
                case _             => base
            }

            defaultMethod.tree match {
              case tree: DefDef => tree.rhs.getOrElse(callDefault)
              case _            => callDefault
            }
          }
          .orElse(
            Option.when(tpe.memberType(field) <:< TypeRepr.of[Option[Any]])('{
              None
            }.asTerm)
          )
          .map(idx -> _)
      }
      .toMap

  private def parseReaderBuilderMacroConfig[T: Type](
      config: Expr[ReaderBuilder[T]]
  ): ReaderBuilderMacroConfig =
    val fields = TypeRepr.of[T].typeSymbol.caseFields.map(_.name)

    def exitExtractionAlreadyDefined(name: String) =
      report.errorAndAbort(s"Field '$name' is already configured")

    def exitFieldExists(name: String) =
      report.errorAndAbort(
        s"Field '$name' exists in your model, use selector or .extract(_.$name).as[...] instead"
      )

    def loop(
        config: Expr[ReaderBuilder[T]],
        acc: ReaderBuilderMacroConfig = ReaderBuilderMacroConfig(Map.empty)
    ): ReaderBuilderMacroConfig =
      config match
        case '{
              ReaderBuilder[T](using
                ${ mirror }: scala.deriving.Mirror.ProductOf[T]
              )
            } =>
          acc
        case '{
              ($rest: ReaderBuilder[T])
                .extract[t](${ SelectedField(field) })
                .as[t1]
                .apply(${ fun })
            } =>
          if acc.extracted.contains(field.name) then
            exitExtractionAlreadyDefined(field.name)
          val lambda =
            Typed(fun.asTerm, TypeTree.of[Any => Any]).asExprOf[Any => Any]
          loop(
            config = rest,
            acc = acc.withExtracted(
              ReaderField.Basic(
                name = field.name,
                tpe = TypeRepr.of[t1],
                extractor = Some((TypeRepr.of[t1], lambda)),
                default = Option.when(TypeRepr.of[t1].isOption)('{ None })
              )
            )
          )
        case '{
              ($rest: ReaderBuilder[T]).fieldStyle(${ style }: FieldStyle)
            } =>
          loop(
            config = rest,
            acc = acc.copy(fieldStyle = Some(style.valueOrAbort))
          )
        case '{ ($rest: ReaderBuilder[T]).strict } =>
          loop(
            config = rest,
            acc = acc.copy(isStrict = true)
          )
        case other =>
          def loopInner(
              term: Term,
              extractors: List[(String, TypeRepr)] = Nil,
              lambda: Expr[Any => Any] = '{ identity[Any] }
          ): ReaderBuilderMacroConfig =
            term match
              case config @ Apply(
                    TypeApply(Select(term, "extract"), List(tpt)),
                    List(SelectedField(name, _))
                  ) =>
                if acc.extracted.contains(name) then
                  exitExtractionAlreadyDefined(name)
                loop(
                  config = term.asExprOf[ReaderBuilder[T]],
                  acc = acc.withExtracted(
                    ReaderField.Extracted(
                      name,
                      tpt.tpe,
                      extractors,
                      lambda,
                      reader = false
                    )
                  )
                )
              case config @ Apply(
                    TypeApply(Select(term, "extractReader"), List(tpt)),
                    List(SelectedField(name, _))
                  ) =>
                if acc.extracted.contains(name) then
                  exitExtractionAlreadyDefined(name)
                loop(
                  config = term.asExprOf[ReaderBuilder[T]],
                  acc = acc.withExtracted(
                    ReaderField.Extracted(
                      name,
                      tpt.tpe,
                      extractors,
                      lambda,
                      reader = true
                    )
                  )
                )
              case Apply(Select(term, "apply"), List(lambda)) =>
                loopInner(
                  term = term,
                  extractors = Nil,
                  lambda = '{
                    ${ lambda.asExprOf[Any] }.asInstanceOf[Any => Any]
                  }
                )
              case Apply(Apply(Select(term, "product"), List(mirror)), _) =>
                loopInner(
                  term = term,
                  extractors = Nil,
                  lambda = '{
                    ${
                      Select
                        .unique(mirror, "fromProduct")
                        .etaExpand(Symbol.spliceOwner)
                        .asExprOf[Any]
                    }.asInstanceOf[Any => Any]
                  }
                )
              case Apply(
                    TypeApply(Select(term, "from" | "and"), List(tpt)),
                    List(SelectedField(name, _))
                  ) =>
                loopInner(
                  term = term,
                  extractors = (name, tpt.tpe) :: extractors,
                  lambda = lambda
                )
              case Apply(
                    TypeApply(Select(term, "from" | "and"), List(tpt)),
                    List(Literal(StringConstant(name)))
                  ) =>
                if fields.contains(name) then exitFieldExists(name)
                loopInner(
                  term = term,
                  extractors = (name, tpt.tpe) :: extractors,
                  lambda = lambda
                )
              case other =>
                report.errorAndAbort(
                  s"Unknown tree. Config must be an inlined given.\nTree: ${other
                      .show(using Printer.TreeStructure)}"
                )

          loopInner(other.asTerm)

    loop(traverseTree(config.asTerm).asExprOf[ReaderBuilder[T]])

  def parseSumConfig[T: Type](config: Expr[JsonConfig[T]]) =
    val tpe = TypeRepr.of[T]
    @scala.annotation.tailrec
    def loop(
        config: Expr[JsonConfig[T]],
        acc: SumMacroConfig = SumMacroConfig()
    ): SumMacroConfig =
      config match
        case '{ JsonConfig[T] } =>
          acc

        case '{
              ($rest: JsonConfig[T]).discriminateBy[fieldType](${
                SelectedField(field)
              })
            } =>
          val fieldTpe = TypeRepr.of[fieldType]
          val symbol = tpe.typeSymbol.fieldMembers
            .find(_.name == field.name)
            .getOrElse(
              report.errorAndAbort(
                s"Selector of type ${fieldTpe.show(using Printer.TypeReprShortCode)} not found in ${tpe
                    .show(using Printer.TypeReprShortCode)}"
              )
            )

          tpe.typeSymbol.children
            .find(child =>
              child.caseFields.contains(symbol.overridingSymbol(child))
            )
            .foreach { child =>
              report.errorAndAbort(
                s"Overriding discriminator field '${symbol.name}' in ${child.typeRef
                    .show(using Printer.TypeReprShortCode)} is prohibited"
              )
            }

          val discriminators = tpe.typeSymbol.children.map { childSymbol =>
            Typed(
              Select(stub(tpe.memberType(childSymbol)), symbol),
              TypeTree.of[fieldType]
            ).asExprOf[fieldType]
          }

          loop(
            rest,
            acc.copy(discriminator =
              Some(
                DiscriminatorConfig(symbol.name, fieldTpe, discriminators)
              )
            )
          )

        case other =>
          report.errorAndAbort(
            s"Unknown tree. Config must be an inlined given.\nTree: ${other.asTerm
                .show(using Printer.TreeStructure)}"
          )

    loop(traverseTree(config.asTerm).asExprOf[JsonConfig[T]])

  private def stub(tpe: TypeRepr): Term =
    import quotes.reflect.*
    val symbol = tpe.typeSymbol
    val constructorFieldsFilledWithNulls: List[List[Term]] =
      symbol.primaryConstructor.paramSymss
        .filterNot(_.exists(_.isType))
        .map(_.map(_.typeRef.widen match {
          case t @ AppliedType(inner, applied) =>
            Select
              .unique('{ null }.asTerm, "asInstanceOf")
              .appliedToTypes(List(inner.appliedTo(tpe.typeArgs)))
          case other =>
            Select
              .unique('{ null }.asTerm, "asInstanceOf")
              .appliedToTypes(List(other))
        }))

    New(TypeTree.ref(symbol))
      .select(symbol.primaryConstructor)
      .appliedToTypes(symbol.typeRef.typeArgs.map(_ => TypeRepr.of[Null]))
      .appliedToArgss(constructorFieldsFilledWithNulls)

  @scala.annotation.tailrec
  private def traverseTree(config: Term): Term =
    config match
      case Inlined(_, _, term)                      => traverseTree(term)
      case Typed(term, _)                           => traverseTree(term)
      case Block(List(ValDef(_, _, Some(term))), _) => traverseTree(term)
      case Block(_, term)                           => traverseTree(term)
      case term                                     => term

  case class SelectedField(name: String, selector: Term)

  object SelectedField:
    def unapply(term: Term): Option[SelectedField] =
      term match
        case lambda @ Lambda(List(ValDef(_, _, _)), Select(_, name)) =>
          Some(SelectedField(name, lambda))
        case _ =>
          None
    def unapply(expr: Expr[Any]): Option[SelectedField] = unapply(expr.asTerm)

  sealed trait WriterField {
    def name: String

    def nameWithStyle: String

    def update: Option[WriterField.Update]

    def tpe: TypeRepr

    def value(root: Term): Term = update match
      case None =>
        Select.unique(root, name)

      case Some(WriterField.Update(lambda, WriterField.Update.What.Root)) =>
        (tpe.asType, root.tpe.asType) match
          case ('[t1], '[t2]) =>
            '{
              ${ lambda.asExprOf[t2 => t1] }.apply(${ root.asExprOf[t2] })
            }.asTerm

      case Some(WriterField.Update(lambda, WriterField.Update.What.Field)) =>
        val field = Select.unique(root, name)
        (tpe.asType, field.tpe.asType) match
          case ('[finalType], '[fieldType]) =>
            '{
              ${ lambda.asExprOf[fieldType => finalType] }.apply(${
                field.asExprOf[fieldType]
              })
            }.asTerm

    def label: Expr[String]
  }

  end WriterField

  object WriterField:
    case class Basic(
        name: String,
        nameWithStyle: String,
        update: Option[WriterField.Update],
        tpe: TypeRepr,
        newName: Option[Expr[Option[String]]]
    ) extends WriterField:
      def label: Expr[String] = newName match
        case Some(newName) =>
          '{ ${ newName }.getOrElse(${ Expr(nameWithStyle) }) }
        case None => Expr(nameWithStyle)

    case class Added(
        name: String,
        label: Expr[String],
        update: Option[Update],
        tpe: TypeRepr
    ) extends WriterField:
      def nameWithStyle = name

    case class Update(lambda: Term, what: Update.What)

    object Update:
      enum What:
        case Root, Field

  case class WriterBuilderMacroConfig(
      delete: Set[String] = Set.empty,
      add: List[WriterField.Added] = Nil,
      update: List[WriterFieldUpdate] = Nil,
      fieldStyle: Option[FieldStyle] = None
  ):
    def withDelete(name: String) = copy(delete = delete + name)
    def withField(field: WriterField.Added) = copy(add = field :: add)

    def withUpdate(op: WriterFieldUpdate) = copy(update = op :: update)

  case class WriterFieldUpdate(
      name: String,
      newName: Expr[Option[String]],
      what: WriterField.Update.What,
      fun: Option[Term],
      to: TypeRepr
  )

  sealed trait ReaderField {
    def name: String
    def tpe: TypeRepr

    def idx: Int
    def default: Option[Expr[Any]]

    def update(
        index: Int,
        default: Option[Expr[Any]],
        fieldStyle: Option[FieldStyle]
    ): ReaderField = this match
      case field: ReaderField.Basic =>
        field.copy(
          idx = index,
          default = field.default.orElse(default),
          name =
            fieldStyle.fold(field.name)(FieldStyle.applyStyle(field.name, _))
        )
      case field: ReaderField.Extracted =>
        field.copy(
          idx = index,
          default = field.default.orElse(default),
          name =
            fieldStyle.fold(field.name)(FieldStyle.applyStyle(field.name, _))
        )

    def defaults(existingFields: Set[String]): List[(String, Expr[Any])] =
      this match
        case field: ReaderField.Basic =>
          field.default.map(field.name -> _).toList
        case field: ReaderField.Extracted =>
          field.default.map(field.name -> _).toList :::
            field.extractors
              .collect {
                case (name, tpe) if !existingFields(name) && tpe.isOption =>
                  name -> '{ (None: Any) }
              }

    def requiredLabels(existingFields: Set[String]): List[String] =
      this match
        case field: ReaderField.Basic => List(field.name)
        case field: ReaderField.Extracted =>
          field.extractors
            .collect {
              case (name, tpe) if !existingFields(name) && !tpe.isOption => name
            }

    def readerTypes(existingFields: Set[String]): List[(String, TypeRepr)] =
      this match
        case ReaderField.Basic(name, tpe, extractor, idx, default) =>
          List(name -> tpe)
        case ReaderField.Extracted(
              name,
              tpe,
              extractors,
              lambda,
              reader,
              idx,
              default
            ) =>
          List(name -> tpe).filterNot(_ => reader) :::
            extractors.collect {
              case (name, tpe) if !existingFields(name) => name -> tpe
            }

  }

  object ReaderField:
    case class Basic(
        name: String,
        tpe: TypeRepr,
        extractor: Option[(TypeRepr, Expr[Any => Any])],
        idx: Int = 0,
        default: Option[Expr[Any]] = None
    ) extends ReaderField

    case class Extracted(
        name: String,
        tpe: TypeRepr,
        extractors: List[(String, TypeRepr)],
        lambda: Expr[Any => Any],
        reader: Boolean,
        idx: Int = 0,
        default: Option[Expr[Any]] = None
    ) extends ReaderField

  case class ReaderBuilderMacroConfig(
      extracted: Map[String, ReaderField] = Map.empty,
      fieldStyle: Option[FieldStyle] = None,
      isStrict: IsStrict = false
  ):
    def withExtracted(field: ReaderField): ReaderBuilderMacroConfig =
      copy(extracted = extracted.updated(field.name, field))

  type IsStrict = Boolean

  case class SumMacroConfig(
      discriminator: Option[DiscriminatorConfig] = None
  )

  case class DiscriminatorConfig(
      label: String,
      tpe: TypeRepr,
      values: List[Expr[?]]
  )

  extension (tpe: TypeRepr)
    def isOption: Boolean = tpe <:< TypeRepr.of[Option[Any]]

  extension [A: Type](exprs: Iterable[A])(using Quotes)
    def exprOfMutableMap[K: ToExpr: Type, V: Type](using
        ev: A <:< (K, Expr[V])
    ): Expr[mutable.Map[K, V]] =
      '{
        mutable.Map(${
          Varargs(
            exprs.map(value => Expr.ofTuple(Expr(value._1) -> value._2)).toSeq
          )
        }: _*)
      }

    def exprOfMap[K: ToExpr: Type, V: Type](using
        ev: A <:< (K, Expr[V])
    ): Expr[Map[K, V]] =
      '{
        Map(${
          Varargs(
            exprs.map(value => Expr.ofTuple(Expr(value._1) -> value._2)).toSeq
          )
        }: _*)
      }

    def exprOfSet(using to: ToExpr[A]): Expr[Set[A]] =
      '{ Set(${ Varargs(exprs.map(value => Expr(value)).toSeq) }: _*) }

  given FromExpr[FieldStyle] = new FromExpr[FieldStyle]:
    override def unapply(x: Expr[FieldStyle])(using
        Quotes
    ): Option[FieldStyle] =
      x match
        case '{ FieldStyle.UpperCase }      => Some(FieldStyle.UpperCase)
        case '{ FieldStyle.LowerCase }      => Some(FieldStyle.LowerCase)
        case '{ FieldStyle.Capitalize }     => Some(FieldStyle.Capitalize)
        case '{ FieldStyle.Uncapitalize }   => Some(FieldStyle.Uncapitalize)
        case '{ FieldStyle.KebabCase }      => Some(FieldStyle.KebabCase)
        case '{ FieldStyle.LowerKebabCase } => Some(FieldStyle.LowerKebabCase)
        case '{ FieldStyle.UpperKebabCase } => Some(FieldStyle.UpperKebabCase)
        case '{ FieldStyle.CapitalizedKebabCase } =>
          Some(FieldStyle.CapitalizedKebabCase)
        case '{ FieldStyle.SnakeCase }      => Some(FieldStyle.SnakeCase)
        case '{ FieldStyle.LowerSnakeCase } => Some(FieldStyle.LowerSnakeCase)
        case '{ FieldStyle.UpperSnakeCase } => Some(FieldStyle.UpperSnakeCase)
        case '{ FieldStyle.CapitalizedSnakeCase } =>
          Some(FieldStyle.CapitalizedSnakeCase)
        case _ => None
