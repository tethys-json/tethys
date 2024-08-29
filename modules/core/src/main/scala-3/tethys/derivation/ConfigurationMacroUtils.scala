package tethys.derivation

import tethys.*
import tethys.readers.FieldName

import scala.annotation.tailrec
import scala.collection.mutable
import scala.compiletime.{constValueTuple, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, FromExpr, Quotes, ToExpr, Type, Varargs}
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.commons.TokenNode
import tethys.derivation.builder.{
  ReaderDerivationConfig,
  WriterDerivationConfig
}

trait ConfigurationMacroUtils:
  given Quotes = quotes
  val quotes: Quotes
  import quotes.reflect.*

  def lookup[T: Type]: Expr[T] =
    Implicits.search(TypeRepr.of[T]) match
      case success: ImplicitSearchSuccess =>
        success.tree.asExprOf[T]
      case failure: ImplicitSearchFailure =>
        // Not sees statements put in a block (e.g. derived instances)
        // So we use summonInline
        '{ summonInline[T] }

  def lookupOpt[T: Type]: Option[Expr[T]] =
    Implicits.search(TypeRepr.of[T]) match
      case success: ImplicitSearchSuccess =>
        Some(success.tree.asExprOf[T])
      case failure: ImplicitSearchFailure =>
        None

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
              update.newName
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
              ($rest: WriterBuilder[T]).fieldStyle(${
                style
              }: tethys.derivation.builder.FieldStyle)
            } =>
          val fieldStyle = legacyFieldStyleToFieldStyle(style).getOrElse {
            report.errorAndAbort(
              s"Can't extract fieldStyle from ${style.asTerm.show(using Printer.TreeShortCode)}"
            )
          }
          (acc.copy(fieldStyle = Some(fieldStyle)), updatedFields)

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
                newName = Some(rename),
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
                newName = None,
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
                newName = Some(rename),
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
                newName = None,
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
                newName = Some(rename),
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
                newName = None,
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
                newName = Some(rename),
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
                newName = None,
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
                newName = Some(rename),
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
          case Some(field: ReaderField.Basic) =>
            val updatedDefault = field.extractor match
              case None => default
              case Some((tpe, lambda)) =>
                Option
                  .when(tpe.isOption)('{ None })
                  .orElse(default)
                  .map(default =>
                    Apply(Select.unique(lambda, "apply"), List(default.asTerm))
                  )
                  .map(_.asExprOf[Any])

            field.update(idx, updatedDefault, macroConfig.fieldStyle)

          case Some(field) =>
            field.update(idx, default, macroConfig.fieldStyle)

          case None =>
            ReaderField
              .Basic(symbol.name, tpe.memberType(symbol), None)
              .update(idx, default, macroConfig.fieldStyle)
      }
    val existingFieldNames = fields.map(_.name).toSet
    val additionalFields = fields
      .collect { case field: ReaderField.Extracted =>
        field.extractors.collect {
          case (name, tpe) if !existingFieldNames(name) =>
            ReaderField.Basic(
              name,
              tpe,
              None,
              -1,
              Option.when(tpe.isOption)('{ None })
            )
        }
      }
      .flatten
      .distinctBy(_.name)
    val allFields = fields ::: additionalFields
    checkLoops(allFields)
    (sortDependencies(allFields), macroConfig.isStrict)

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

          loop(
            config = rest,
            acc = acc.withExtracted(
              ReaderField.Basic(
                name = field.name,
                tpe = TypeRepr.of[t],
                extractor = Some(
                  (TypeRepr.of[t1], Typed(fun.asTerm, TypeTree.of[t1 => t]))
                )
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

        case '{
              ($rest: ReaderBuilder[T]).fieldStyle(${
                style
              }: tethys.derivation.builder.FieldStyle)
            } =>
          val fieldStyle = legacyFieldStyleToFieldStyle(style).getOrElse {
            report.errorAndAbort(
              s"Can't extract fieldStyle from ${style.asTerm.show(using Printer.TreeShortCode)}"
            )
          }
          loop(
            config = rest,
            acc = acc.copy(fieldStyle = Some(fieldStyle))
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
              lambda: Term = '{ identity[Any] }.asTerm
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
                  acc = acc
                    .withExtracted(
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
                  lambda = lambda
                )
              case Apply(Apply(Select(term, "product"), List(mirror)), _) =>
                loopInner(
                  term = term,
                  extractors = Nil,
                  lambda = Select
                    .unique(mirror, "fromProduct")
                    .etaExpand(Symbol.spliceOwner)
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

  def parseSumConfig[T: Type]: SumMacroConfig =
    val tpe = TypeRepr.of[T]
    val tpt = TypeTree.of[T]
    val annotation = TypeRepr.of[selector].typeSymbol
    val selectors = tpe.typeSymbol.primaryConstructor.paramSymss.flatten
      .filter(_.hasAnnotation(annotation))

    selectors match
      case constructorSymbol :: Nil =>
        val symbol = tpe.typeSymbol.fieldMembers
          .find(_.name == constructorSymbol.name)
          .getOrElse(
            report.errorAndAbort(
              s"Not found symbol corresponding to constructor symbol ${constructorSymbol.name}"
            )
          )

        val discriminators: List[Term] = getAllChildren(tpe).map {
          case tpe: TypeRef =>
            Select(stub(tpe), symbol)
          case tpe: TermRef =>
            Select(Ref(tpe.termSymbol), symbol)
          case tpe =>
            report.errorAndAbort(s"Unknown tpe: $tpe")
        }
        SumMacroConfig(
          Some(
            DiscriminatorConfig(
              symbol.name,
              tpe.memberType(symbol),
              discriminators
            )
          )
        )

      case Nil =>
        SumMacroConfig(None)

      case multiple =>
        report.errorAndAbort(
          s"Only one field can be a selector. Found ${multiple.map(_.name).mkString(", ")}"
        )

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

  private def typeReprsOf[Ts: Type]: List[TypeRepr] =
    Type.of[Ts] match
      case '[EmptyTuple] => Nil
      case '[t *: ts]    => TypeRepr.of[t] :: typeReprsOf[ts]

  def getAllChildren(tpe: TypeRepr): List[TypeRepr] =
    tpe.asType match
      case '[t] =>
        Expr.summon[scala.deriving.Mirror.Of[t]] match
          case Some('{
                $m: scala.deriving.Mirror.SumOf[t] {
                  type MirroredElemTypes = subs
                }
              }) =>
            typeReprsOf[subs].flatMap(getAllChildren)
          case _ =>
            List(tpe)

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
        newName: Option[Expr[String]]
    ) extends WriterField:
      def label: Expr[String] = newName.getOrElse(Expr(nameWithStyle))

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
      newName: Option[Expr[String]],
      what: WriterField.Update.What,
      fun: Option[Term],
      to: TypeRepr
  )

  sealed trait ReaderField {
    def name: String
    def tpe: TypeRepr
    def reader: Boolean

    def initializeFieldCase(
        readers: Map[TypeRepr, Ref],
        it: Expr[TokenIterator],
        fieldName: Expr[FieldName]
    ): Option[CaseDef] =
      this match
        case _: ReaderField.Basic =>
          Some(
            readerTpe.get.asType match {
              case '[t] =>
                CaseDef(
                  Literal(StringConstant(name)),
                  None,
                  Block(
                    init {
                      val reader = readers
                        .get(readerTpe.get)
                        .fold(lookup[JsonReader[t]])(_.asExprOf[JsonReader[t]])
                      '{
                        ${ reader }.read(${ it })(
                          ${ fieldName }.appendFieldName(${ Expr(name) })
                        )
                      }.asTerm
                    },
                    '{}.asTerm
                  )
                )
            }
          )

        case _: ReaderField.Extracted =>
          iteratorRef.map { iteratorRef =>
            CaseDef(
              Literal(StringConstant(name)),
              None,
              Block(
                initIterator('{ ${ it }.collectExpression() }.asTerm),
                '{}.asTerm
              )
            )
          }

    lazy val (initialize, ref, initRef, iteratorRef) = {
      val flags =
        default.fold(Flags.Deferred | Flags.Mutable)(_ => Flags.Mutable)
      val symbol = Symbol.newVal(
        Symbol.spliceOwner,
        s"${name}Var",
        tpe,
        flags,
        Symbol.noSymbol
      )
      val initSymbol = Symbol.newVal(
        Symbol.spliceOwner,
        s"${name}Init",
        TypeRepr.of[Boolean],
        Flags.Mutable,
        Symbol.noSymbol
      )
      val stat = ValDef(symbol, default.map(_.asTerm))
      val initStat = ValDef(initSymbol, Some('{ false }.asTerm))
      val iteratorSymbol = Option.when(reader)(
        Symbol.newVal(
          Symbol.spliceOwner,
          s"${name}Iterator",
          TypeRepr.of[TokenIterator],
          Flags.Mutable | Flags.Deferred,
          Symbol.noSymbol
        )
      )
      val iteratorStat = iteratorSymbol.map(ValDef(_, None))
      val iteratorRef = iteratorStat.map(stat => Ref(stat.symbol))
      (
        List(stat, initStat) ++ iteratorStat,
        Ref(stat.symbol),
        Ref(initStat.symbol),
        iteratorRef
      )
    }

    def idx: Int
    def default: Option[Expr[Any]]
    def readerTpe: Option[TypeRepr] = this match
      case ReaderField.Basic(name, tpe, extractor, idx, default) =>
        Some(extractor.map(_._1).getOrElse(tpe))
      case field: ReaderField.Extracted if field.reader =>
        None
      case field: ReaderField.Extracted =>
        Some(field.tpe)

    def init(value: Term): List[Statement] = this match
      case ReaderField.Basic(_, _, None, _, _) =>
        List(
          Assign(ref, value),
          Assign(initRef, '{ true }.asTerm)
        )

      case ReaderField.Basic(_, _, Some((_, lambda)), _, _) =>
        List(
          Assign(ref, Apply(Select.unique(lambda, "apply"), List(value))),
          Assign(initRef, '{ true }.asTerm)
        )
      case extracted: ReaderField.Extracted =>
        List(
          Assign(ref, value),
          Assign(initRef, '{ true }.asTerm)
        )

    def initIterator(value: Term): List[Statement] = iteratorRef
      .map { ref =>
        List(
          Assign(ref, value),
          Assign(initRef, '{ true }.asTerm)
        )
      }
      .getOrElse(Nil)

    def update(
        index: Int,
        default: Option[Expr[Any]],
        fieldStyle: Option[FieldStyle]
    ): ReaderField = this match
      case field: ReaderField.Basic =>
        field.copy(
          idx = index,
          default = default,
          name =
            fieldStyle.fold(field.name)(FieldStyle.applyStyle(field.name, _))
        )
      case field: ReaderField.Extracted =>
        field.copy(
          idx = index,
          default = default,
          name =
            fieldStyle.fold(field.name)(FieldStyle.applyStyle(field.name, _))
        )
  }

  object ReaderField:
    case class Basic(
        name: String,
        tpe: TypeRepr,
        extractor: Option[(TypeRepr, Term)],
        idx: Int = 0,
        default: Option[Expr[Any]] = None
    ) extends ReaderField:
      def reader = false

    case class Extracted(
        name: String,
        tpe: TypeRepr,
        extractors: List[(String, TypeRepr)],
        lambda: Term,
        reader: Boolean,
        idx: Int = 0,
        default: Option[Expr[Any]] = None
    ) extends ReaderField:
      def extract(
          fields: Map[String, Ref],
          fieldName: Expr[FieldName]
      ): List[Statement] =
        val term = extractors match
          case (depName, _) :: Nil =>
            Apply(Select.unique(lambda, "apply"), List(fields(depName)))
          case _ =>
            val value = extractors
              .map((name, _) => fields(name))
              .foldRight[Term]('{ EmptyTuple }.asTerm) { (el, acc) =>
                Select
                  .unique(acc, "*:")
                  .appliedToTypes(List(el.tpe, acc.tpe))
                  .appliedToArgs(List(el))
              }
            Select.unique(lambda, "apply").appliedToArgs(List(value))

        iteratorRef match
          case Some(iteratorRef) =>
            val reader = Typed(term, TypeTree.of[JsonReader[Any]])
              .asExprOf[JsonReader[Any]]
            val it = '{
              if ${ initRef.asExprOf[Boolean] } then
                ${ iteratorRef.asExprOf[TokenIterator] }
              else QueueIterator(List(TokenNode.NullValueNode))
            }
            val value = '{
              ${ reader }.read(${ it })(${ fieldName }.appendFieldName(${
                Expr(name)
              }))
            }
            init(value.asTerm)
          case None =>
            init(term)

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
      values: List[Term]
  )

  extension (tpe: TypeRepr)
    def isOption: Boolean = tpe <:< TypeRepr.of[Option[Any]]

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

  @deprecated
  def legacyFieldStyleToFieldStyle(
      x: Expr[tethys.derivation.builder.FieldStyle]
  ): Option[FieldStyle] =
    x match
      case '{ tethys.derivation.builder.FieldStyle.UpperCase } =>
        Some(FieldStyle.UpperCase)
      case '{ tethys.derivation.builder.FieldStyle.uppercase } =>
        Some(FieldStyle.UpperCase)
      case '{ tethys.derivation.builder.FieldStyle.LowerCase } =>
        Some(FieldStyle.LowerCase)
      case '{ tethys.derivation.builder.FieldStyle.lowercase } =>
        Some(FieldStyle.LowerCase)
      case '{ tethys.derivation.builder.FieldStyle.Capitalize } =>
        Some(FieldStyle.Capitalize)
      case '{ tethys.derivation.builder.FieldStyle.capitalize } =>
        Some(FieldStyle.Capitalize)
      case '{ tethys.derivation.builder.FieldStyle.Uncapitalize } =>
        Some(FieldStyle.Uncapitalize)
      case '{ tethys.derivation.builder.FieldStyle.uncapitalize } =>
        Some(FieldStyle.Uncapitalize)
      case '{ tethys.derivation.builder.FieldStyle.KebabCase } =>
        Some(FieldStyle.KebabCase)
      case '{ tethys.derivation.builder.FieldStyle.kebabCase } =>
        Some(FieldStyle.KebabCase)
      case '{ tethys.derivation.builder.FieldStyle.LowerKebabCase } =>
        Some(FieldStyle.LowerKebabCase)
      case '{ tethys.derivation.builder.FieldStyle.lowerKebabCase } =>
        Some(FieldStyle.LowerKebabCase)
      case '{ tethys.derivation.builder.FieldStyle.UpperKebabCase } =>
        Some(FieldStyle.UpperKebabCase)
      case '{ tethys.derivation.builder.FieldStyle.upperKebabCase } =>
        Some(FieldStyle.UpperKebabCase)
      case '{ tethys.derivation.builder.FieldStyle.CapitalizedKebabCase } =>
        Some(FieldStyle.CapitalizedKebabCase)
      case '{ tethys.derivation.builder.FieldStyle.capitalizedKebabCase } =>
        Some(FieldStyle.CapitalizedKebabCase)
      case '{ tethys.derivation.builder.FieldStyle.SnakeCase } =>
        Some(FieldStyle.SnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.snakeCase } =>
        Some(FieldStyle.SnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.LowerSnakeCase } =>
        Some(FieldStyle.LowerSnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.lowerSnakeCase } =>
        Some(FieldStyle.LowerSnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.UpperSnakeCase } =>
        Some(FieldStyle.UpperSnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.upperSnakeCase } =>
        Some(FieldStyle.UpperSnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.CapitalizedSnakeCase } =>
        Some(FieldStyle.CapitalizedSnakeCase)
      case '{ tethys.derivation.builder.FieldStyle.capitalizedSnakeCase } =>
        Some(FieldStyle.CapitalizedSnakeCase)
      case _ => None

  @deprecated
  def parseLegacyReaderDerivationConfig[T: Type](
      config: Expr[ReaderDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  ): Expr[ReaderBuilder[T]] =
    config match
      case '{
            ReaderDerivationConfig.withFieldStyle(${ fieldStyle }: FieldStyle)
          } =>
        '{ ReaderBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{
            ReaderDerivationConfig.withFieldStyle(${
              fieldStyle
            }: tethys.derivation.builder.FieldStyle)
          } =>
        '{ ReaderBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{
            ReaderDerivationConfig.empty.withFieldStyle(${
              fieldStyle
            }: FieldStyle)
          } =>
        '{ ReaderBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{
            ReaderDerivationConfig.empty.withFieldStyle(${
              fieldStyle
            }: tethys.derivation.builder.FieldStyle)
          } =>
        '{ ReaderBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{ ReaderDerivationConfig.strict } =>
        '{ ReaderBuilder[T](using ${ mirror }).strict }

      case '{ ReaderDerivationConfig.empty.strict } =>
        '{ ReaderBuilder[T](using ${ mirror }).strict }

      case '{
            ReaderDerivationConfig
              .withFieldStyle(${ fieldStyle }: FieldStyle)
              .strict
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig
              .withFieldStyle(${
                fieldStyle
              }: tethys.derivation.builder.FieldStyle)
              .strict
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig.strict.withFieldStyle(${
              fieldStyle
            }: FieldStyle)
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig.strict.withFieldStyle(${
              fieldStyle
            }: tethys.derivation.builder.FieldStyle)
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig.empty
              .withFieldStyle(${ fieldStyle }: FieldStyle)
              .strict
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig.empty
              .withFieldStyle(${
                fieldStyle
              }: tethys.derivation.builder.FieldStyle)
              .strict
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig.empty.strict.withFieldStyle(${
              fieldStyle
            }: FieldStyle)
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case '{
            ReaderDerivationConfig.empty.strict.withFieldStyle(${
              fieldStyle
            }: tethys.derivation.builder.FieldStyle)
          } =>
        '{
          ReaderBuilder[T](using ${ mirror }).strict.fieldStyle(${ fieldStyle })
        }

      case other =>
        report.errorAndAbort(
          s"Unknown tree: ${other.asTerm.show(using Printer.TreeShortCode)}"
        )

  @deprecated
  def parseLegacyWriterDerivationConfig[T: Type](
      config: Expr[WriterDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  ): Expr[WriterBuilder[T]] =
    config match
      case '{
            WriterDerivationConfig.withFieldStyle(${ fieldStyle }: FieldStyle)
          } =>
        '{ WriterBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{
            WriterDerivationConfig.withFieldStyle(${
              fieldStyle
            }: tethys.derivation.builder.FieldStyle)
          } =>
        '{ WriterBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{
            WriterDerivationConfig.empty.withFieldStyle(${
              fieldStyle
            }: FieldStyle)
          } =>
        '{ WriterBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case '{
            WriterDerivationConfig.empty.withFieldStyle(${
              fieldStyle
            }: tethys.derivation.builder.FieldStyle)
          } =>
        '{ WriterBuilder[T](using ${ mirror }).fieldStyle(${ fieldStyle }) }

      case other =>
        report.errorAndAbort(
          s"Unknown tree: ${other.asTerm.show(using Printer.TreeShortCode)}"
        )

  def parseLegacyDiscriminator[T: Type](
      config: Expr[WriterDerivationConfig]
  ): DiscriminatorConfig =
    val name: String = config match
      case '{ WriterDerivationConfig.withDiscriminator($name: String) } =>
        name.valueOrAbort
      case '{ WriterDerivationConfig.empty.withDiscriminator($name: String) } =>
        name.valueOrAbort
      case other =>
        report.errorAndAbort(
          s"Unknown tree: ${other.asTerm.show(using Printer.TreeShortCode)}"
        )

    DiscriminatorConfig(name, TypeRepr.of[String], Nil)
