package tethys.derivation.impl.builder

import tethys.derivation.semiauto.*
import tethys.derivation.impl.FieldStyle
import tethys.derivation.builder.{FieldStyle as ConfigFieldStyle, ReaderDerivationConfig, ReaderDescription}
import tethys.derivation.impl.Reflection
import scala.quoted.*

import tethys.derivation.builder

trait ReaderBuilderUtils extends Reflection {
  import context.reflect.*

  case class MacroReaderDescription(config: Expr[ReaderDerivationConfig], operations: Seq[ReaderMacroOperation]) {
    def lift[T: Type]: Expr[ReaderDescription[T]] =
      '{ReaderDescription[T]($config, ${Expr.ofSeq(operations.map(_.lift))})}
  }
  object MacroReaderDescription {
    def unlift[T: Type](wd: Expr[ReaderDescription[T]]): MacroReaderDescription = {
      val withoutInlining = (wd.asTerm.underlying match {
        case Inlined(_, _, expansion) => expansion
        case notInlined => notInlined
      }).asExprOf[ReaderDescription[T]]

      withoutInlining match {
        case '{ ReaderDescription.apply[T] ($config, ${Varargs(operations)}) } =>
          MacroReaderDescription(config, operations.map(ReaderMacroOperation.unlift[T]))
      }
    }
  }

  sealed trait Field { self =>
    def name: String
    def tpe: TypeRepr

    def lift: Expr[ReaderDescription.Field[?]] = self match {
      case Field.ClassField(name, tpe) =>
        tpe.asType match { case '[t] =>
          '{ ReaderDescription.Field.ClassField[t](${Expr(name)}) }
        }
      case Field.RawField(name, tpe) =>
        tpe.asType match { case '[t] =>
          '{ ReaderDescription.Field.RawField[t](${Expr(name)}) }
        }
    }
  }
  object Field {
    final case class ClassField(name: String, tpe: TypeRepr) extends Field
    final case class RawField(name: String, tpe: TypeRepr) extends Field

    def unlift(rdField: Term): Field = rdField.asExpr match {
      case '{ ReaderDescription.Field.ClassField.apply[tpe]($name) } =>
        Field.ClassField(name.valueOrAbort, TypeRepr.of[tpe])

      case '{ ReaderDescription.Field.RawField.apply[tpe]($name) } =>
        Field.RawField(name.valueOrAbort, TypeRepr.of[tpe])

      case '{ ReaderFieldStringOps($name).as[tpe] } =>
        Field.RawField(name.valueOrAbort, TypeRepr.of[tpe])

      case '{ ReaderFieldSymbolOps(scala.Symbol.apply($name)).as[tpe] } =>
        Field.RawField(name.valueOrAbort, TypeRepr.of[tpe])
    }

    def fromBuilderField(bf: BuilderField): Field =
      Field.ClassField(bf.name, bf.tpe)
  }

  sealed trait ReaderMacroOperation { self =>
    def field: String

    def lift: Expr[ReaderDescription.BuilderOperation] = self match {
      case ReaderMacroOperation.ExtractFieldAs(field, tpe, as, fun) =>
        (tpe.asType, as.asType) match {
          case ('[tpe], '[as]) =>
            '{ ReaderDescription.BuilderOperation.ExtractFieldAs[as, tpe](${Expr(field)}, ${fun.asExprOf[Function[as, tpe]]}) }
        }

      case ReaderMacroOperation.ExtractFieldValue(field, from, fun) =>
        '{ ReaderDescription.BuilderOperation.ExtractFieldValue(${Expr(field)}, ${Expr.ofSeq(from.map(_.lift))}, ${fun.asExprOf[Any]}) }

      case ReaderMacroOperation.ExtractFieldReader(field, from, fun) =>
        '{ ReaderDescription.BuilderOperation.ExtractFieldReader(${Expr(field)}, ${Expr.ofSeq(from.map(_.lift))}, ${fun.asExprOf[Any]}) }
    }
  }
  object ReaderMacroOperation {
    final case class ExtractFieldAs(field: String, tpe: TypeRepr, as: TypeRepr, fun: Term) extends ReaderMacroOperation
    final case class ExtractFieldValue(field: String, from: Seq[Field], fun: Term) extends ReaderMacroOperation
    final case class ExtractFieldReader(field: String, from: Seq[Field], fun: Term) extends ReaderMacroOperation

    def unlift[T: Type](bo: Expr[ReaderDescription.BuilderOperation]): ReaderMacroOperation =
      bo match {
        case '{ ReaderDescription.BuilderOperation.ExtractFieldAs.apply[as, tpe]($field, $fun) } =>
          ReaderMacroOperation.ExtractFieldAs(field.valueOrAbort, TypeRepr.of[tpe], TypeRepr.of[as], fun.asTerm)
        case '{ ReaderDescription.BuilderOperation.ExtractFieldValue.apply($field, ${Varargs(from)}, $fun) } =>
          ReaderMacroOperation.ExtractFieldValue(field.valueOrAbort, from.map(f => Field.unlift(f.asTerm)), fun.asTerm)
        case '{ ReaderDescription.BuilderOperation.ExtractFieldReader.apply($field, ${Varargs(from)}, $fun) } =>
          ReaderMacroOperation.ExtractFieldReader(field.valueOrAbort, from.map(f => Field.unlift(f.asTerm)), fun.asTerm)
      }
  }

  protected def evalReaderConfig(configExpr: Expr[ReaderDerivationConfig]): (Option[FieldStyle], Boolean) = {
    def parseConfigExpr(
        confExpr: Expr[ReaderDerivationConfig],
        fieldStyleExpr: Option[Expr[ConfigFieldStyle]],
        isStrictExpr: Option[Expr[Boolean]]
    ): (Option[Expr[ConfigFieldStyle]], Option[Expr[Boolean]]) =
      confExpr match {
        case '{ ReaderDerivationConfig.empty } =>
          (fieldStyleExpr, isStrictExpr)

        case '{ ReaderDerivationConfig.apply(None, $isStrict) } =>
          (fieldStyleExpr, isStrictExpr.orElse(Some(isStrict)))

        case '{ ReaderDerivationConfig.apply(Some($fieldStyle), $isStrict) } =>
          (fieldStyleExpr.orElse(Some(fieldStyle)), isStrictExpr.orElse(Some(isStrict)))

        case '{ ReaderDerivationConfig.withFieldStyle($fieldStyle) } =>
          (fieldStyleExpr.orElse(Some(fieldStyle)), isStrictExpr)

        case '{ ReaderDerivationConfig.strict } =>
          (fieldStyleExpr, Some(Expr(true)))

        case '{ ($config: ReaderDerivationConfig).withFieldStyle($fieldStyle) } =>
          parseConfigExpr(config, fieldStyleExpr.orElse(Some(fieldStyle)), isStrictExpr)

        case '{ ($config: ReaderDerivationConfig).strict } =>
          parseConfigExpr(config, fieldStyleExpr, Some(Expr(true)))

        case _ => report.errorAndAbort(s"Config parsing error. Unknown expr: ${confExpr.asTerm.show}")
      }

    val (fieldStyleExpr, isStrictExpr) = parseConfigExpr(configExpr, None, None)

    val fieldStyle: Option[FieldStyle] = fieldStyleExpr.map { fs =>
      fs.asTerm.underlying.asExprOf[ConfigFieldStyle] match {
        case '{ ConfigFieldStyle.LowerCase } => FieldStyle.lowercase
        case '{ ConfigFieldStyle.UpperCase } => FieldStyle.uppercase

        case '{ ConfigFieldStyle.Capitalize }   => FieldStyle.capitalize
        case '{ ConfigFieldStyle.Uncapitalize } => FieldStyle.uncapitalize

        case '{ ConfigFieldStyle.KebabCase }              => FieldStyle.kebabcase
        case '{ ConfigFieldStyle.LowerKebabCase }         => FieldStyle.lowerKebabcase
        case '{ ConfigFieldStyle.UpperKebabCase }         => FieldStyle.upperKebabcase
        case '{ ConfigFieldStyle.CapitalizedKebabCase }   => FieldStyle.capitalizedKebabCase
        case '{ ConfigFieldStyle.UncapitalizedKebabCase } => FieldStyle.uncapitalizedKebabCase

        case '{ ConfigFieldStyle.SnakeCase }              => FieldStyle.snakecase
        case '{ ConfigFieldStyle.LowerSnakeCase }         => FieldStyle.lowerSnakecase
        case '{ ConfigFieldStyle.UpperSnakeCase }         => FieldStyle.upperSnakecase
        case '{ ConfigFieldStyle.CapitalizedSnakeCase }   => FieldStyle.capitalizedSnakecase
        case '{ ConfigFieldStyle.UncapitalizedSnakeCase } => FieldStyle.uncapitalizedSnakecase

        case _ => report.errorAndAbort("fieldStyle in reader config can not be computed in compile-time")
      }
    }

    val isStrict = isStrictExpr.map(
      _.asTerm
        .underlying
        .asExprOf[Boolean]
        .value
        .getOrElse(report.errorAndAbort("isStrict in reader config can not be computed in compile-time"))
    ).getOrElse(false)

    (fieldStyle, isStrict)
  }
}
