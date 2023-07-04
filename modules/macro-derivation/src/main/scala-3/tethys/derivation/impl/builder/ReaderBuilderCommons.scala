package tethys.derivation.impl.builder

import tethys.derivation.builder.{ReaderBuilder, ReaderDerivationConfig, ReaderDescription}

import scala.quoted.*

trait ReaderBuilderCommons extends ReaderBuilderUtils {
  import context.reflect.*

  protected def convertReaderBuilder[T <: Product : Type](
    builder: Expr[ReaderBuilder[T]]
  ): Expr[ReaderDescription[T]] = {
    val withoutInlining = (builder.asTerm match {
      case Inlined(_, _, expansion) => expansion
      case notInlined => notInlined
    }).asExprOf[ReaderBuilder[T]]

    val description = extractDescription(withoutInlining)
    description.lift[T]
  }

  protected lazy val emptyReaderConfig: Expr[ReaderDerivationConfig] =
    '{ ReaderDerivationConfig.empty }

  private def extractDescription[T <: Product: Type](expr: Expr[ReaderBuilder[T]]): MacroReaderDescription = expr match {
    // ===== ROOT =====
    case '{ ReaderBuilder.apply[t] } =>
      MacroReaderDescription(emptyReaderConfig, Seq())

    // ===== FieldAs =====
    case '{ ($rest: ReaderBuilder[T]).extract[tpe](${BuilderField(f)}).as[as].apply($fun) } =>
      val description = extractDescription(rest)
      description.copy(operations =
        description.operations :+ ReaderMacroOperation.ExtractFieldAs(
          f.name, TypeRepr.of[tpe], TypeRepr.of[as], fun.asTerm
        )
      )

    // ===== FieldStyle =====
    case '{ ($rest: ReaderBuilder[T]).fieldStyle($style) } =>
      val description = extractDescription(rest)
      description.copy(config = '{ (${ description.config }: ReaderDerivationConfig).withFieldStyle($style) })

    // ===== isStrict =====
    case '{ ($rest: ReaderBuilder[T]).strict } =>
      val description = extractDescription(rest)
      description.copy(config = '{ (${ description.config }: ReaderDerivationConfig).strict })

    case other => other.asTerm match {
      // ===== FieldValue =====

      // q"${rest: Tree}.extract[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).apply(${fun: Tree})"
      case Apply(Select(Apply(TypeApply(Select(Apply(TypeApply(Select(rest, "extract"), _), List(BuilderField(f))), "from"), _), fs), "apply"), List(fun)) =>
        val description = extractDescription(rest.asExprOf[ReaderBuilder[T]])
        val fsFields = fs.map(fTerm =>
          BuilderField.unapply(fTerm).fold(Field.unlift(fTerm))(Field.fromBuilderField)
        )
        description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldValue(
          f.name, fsFields, fun
        ))

      // q"${rest: Tree}.extract[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).and[..$_](..${ands: Seq[Field]}).apply(${fun: Tree})"
      case Apply(Select(Apply(TypeApply(Select(Apply(TypeApply(Select(Apply(TypeApply(Select(rest, "extract"), _), List(BuilderField(f))), "from"), _), fs), "and"), _), ands), "apply"), List(fun)) =>
        val description = extractDescription(rest.asExprOf[ReaderBuilder[T]])
        val fsFields = fs.map(fTerm =>
          BuilderField.unapply(fTerm).fold(Field.unlift(fTerm))(Field.fromBuilderField)
        )
        val andsFields = ands.map(fTerm =>
          BuilderField.unapply(fTerm).fold(Field.unlift(fTerm))(Field.fromBuilderField)
        )
        description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldValue(
          f.name, fsFields ++ andsFields, fun
        ))

      // ===== FieldReader =====

      // q"${rest: Tree}.extractReader[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).apply(${fun: Tree})"
      case Apply(Select(Apply(TypeApply(Select(Apply(TypeApply(Select(rest, "extractReader"), _), List(BuilderField(f))), "from"), _), fs), "apply"), List(fun)) =>
        val description = extractDescription(rest.asExprOf[ReaderBuilder[T]])
        val fsFields = fs.map(fTerm =>
          BuilderField.unapply(fTerm).fold(Field.unlift(fTerm))(Field.fromBuilderField)
        )
        description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldReader(
          f.name, fsFields, fun
        ))


      // q"${rest: Tree}.extractReader[$tpe](${f: BuilderField}).from[..$_](..${fs: Seq[Field]}).and[..$_](..${ands: Seq[Field]}).apply(${fun: Tree})"
      case Apply(Select(Apply(TypeApply(Select(Apply(TypeApply(Select(Apply(TypeApply(Select(rest, "extractReader"), _), List(BuilderField(f))), "from"), _), fs), "and"), _), ands), "apply"), List(fun)) =>
        val description = extractDescription(rest.asExprOf[ReaderBuilder[T]])
        val fsFields = fs.map(fTerm =>
          BuilderField.unapply(fTerm).fold(Field.unlift(fTerm))(Field.fromBuilderField)
        )
        val andsFields = ands.map(fTerm =>
          BuilderField.unapply(fTerm).fold(Field.unlift(fTerm))(Field.fromBuilderField)
        )
        description.copy(operations = description.operations :+ ReaderMacroOperation.ExtractFieldReader(
          f.name, fsFields ++ andsFields, fun
        ))

      // ===== NOPE =====
      case _ => report.errorAndAbort(s"unknown tree: ${expr.asTerm.show}")
    }
  }
}
