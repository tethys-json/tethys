package tethys.derivation.impl.builder

import scala.quoted.*

import tethys.derivation.builder.{FieldStyle as ConfigFieldStyle, WriterDerivationConfig, WriterDescription}
import tethys.derivation.impl.{FieldStyle, Reflection}

trait WriterBuilderUtils extends Reflection {
  import context.reflect.*

  case class MacroWriteDescription(
      tpe: TypeRepr,
      config: Expr[WriterDerivationConfig],
      operations: Seq[WriterMacroOperation]
  ) { self =>
    def lift[T: Type]: Expr[WriterDescription[T]] =
      '{ WriterDescription[T]($config, ${ Expr.ofSeq(operations.map(_.lift[T])) }) }
  }

  object MacroWriteDescription {
    def unlift[T: Type](wd: Expr[WriterDescription[T]]): MacroWriteDescription = {
      val withoutInlining = (wd.asTerm.underlying match {
        case Inlined(_, _, expansion) => expansion
        case notInlined               => notInlined
      }).asExprOf[WriterDescription[T]]

      withoutInlining match {
        case '{ WriterDescription.apply[T]($config, ${ Varargs(operations) }) } =>
          MacroWriteDescription(TypeRepr.of[T], config, operations.map(WriterMacroOperation.unlift[T]))
      }
    }
  }

  sealed trait WriterMacroOperation { self =>
    def lift[T: Type]: Expr[WriterDescription.BuilderOperation[T]] =
      self match {
        case WriterMacroOperation.Remove(tpe, field) =>
          tpe.asType match {
            case '[tpe] =>
              '{ WriterDescription.BuilderOperation.Remove.apply[tpe](${ Expr(field) }) }
                .asInstanceOf[Expr[WriterDescription.BuilderOperation[T]]]
          }

        case WriterMacroOperation.Update(tpe, field, name, fun, from, to) => {
          (tpe.asType, from.asType, to.asType) match {
            case ('[tpe], '[from], '[to]) =>
              '{
                WriterDescription.BuilderOperation.Update.apply[tpe, from, to](
                  ${ Expr(field) },
                  $name,
                  ${ fun.asExprOf[Function[from, to]] }
                )
              }.asInstanceOf[Expr[WriterDescription.BuilderOperation[T]]]
          }
        }

        case WriterMacroOperation.UpdateFromRoot(tpe, field, name, fun, to) =>
          (tpe.asType, to.asType) match {
            case ('[tpe], '[to]) =>
              '{
                WriterDescription.BuilderOperation.UpdateFromRoot
                  .apply[tpe, to](${ Expr(field) }, $name, ${ fun.asExprOf[Function[tpe, to]] })
              }.asInstanceOf[Expr[WriterDescription.BuilderOperation[T]]]
          }

        case WriterMacroOperation.UpdatePartial(tpe, field, name, fun, to) =>
          (tpe.asType, to.asType) match {
            case ('[tpe], '[to]) =>
              '{
                WriterDescription.BuilderOperation.UpdatePartial
                  .apply[tpe, to](${ Expr(field) }, $name, ${ fun.asExprOf[PartialFunction[to, Any]] })
              }.asInstanceOf[Expr[WriterDescription.BuilderOperation[T]]]
          }

        case WriterMacroOperation.UpdatePartialFromRoot(tpe, field, name, fun) =>
          tpe.asType match {
            case '[tpe] =>
              '{
                WriterDescription.BuilderOperation.UpdatePartialFromRoot
                  .apply[tpe](${ Expr(field) }, $name, ${ fun.asExprOf[PartialFunction[tpe, Any]] })
              }.asInstanceOf[Expr[WriterDescription.BuilderOperation[T]]]
          }

        case WriterMacroOperation.Add(tpe, field, fun, to) =>
          (tpe.asType, to.asType) match {
            case ('[tpe], '[to]) =>
              '{ WriterDescription.BuilderOperation.Add.apply[tpe, to]($field, ${ fun.asExprOf[Function[tpe, to]] }) }
                .asInstanceOf[Expr[WriterDescription.BuilderOperation[T]]]
          }
      }
  }

  object WriterMacroOperation {
    case class Remove(tpe: TypeRepr, field: String) extends WriterMacroOperation
    case class Update(tpe: TypeRepr, field: String, name: Expr[Option[String]], fun: Term, from: TypeRepr, to: TypeRepr)
        extends WriterMacroOperation
    case class UpdateFromRoot(tpe: TypeRepr, field: String, name: Expr[Option[String]], fun: Term, to: TypeRepr)
        extends WriterMacroOperation
    case class UpdatePartial(tpe: TypeRepr, field: String, name: Expr[Option[String]], fun: Term, from: TypeRepr)
        extends WriterMacroOperation
    case class UpdatePartialFromRoot(tpe: TypeRepr, field: String, name: Expr[Option[String]], fun: Term)
        extends WriterMacroOperation
    case class Add(tpe: TypeRepr, field: Expr[String], fun: Term, to: TypeRepr) extends WriterMacroOperation

    def unlift[T: Type](bo: Expr[WriterDescription.BuilderOperation[T]]): WriterMacroOperation =
      bo match {
        case '{ WriterDescription.BuilderOperation.Remove.apply[tpe]($field) } =>
          WriterMacroOperation.Remove(TypeRepr.of[tpe], field.valueOrAbort)

        case '{ WriterDescription.BuilderOperation.Update.apply[tpe, from, to]($field, $name, $fun) } =>
          WriterMacroOperation.Update(
            TypeRepr.of[tpe],
            field.valueOrAbort,
            name,
            fun.asTerm,
            TypeRepr.of[from],
            TypeRepr.of[to]
          )

        case '{ WriterDescription.BuilderOperation.UpdateFromRoot.apply[tpe, to]($field, $name, $fun) } =>
          WriterMacroOperation.UpdateFromRoot(TypeRepr.of[tpe], field.valueOrAbort, name, fun.asTerm, TypeRepr.of[to])

        case '{ WriterDescription.BuilderOperation.UpdatePartial.apply[tpe, from]($field, $name, $fun) } =>
          WriterMacroOperation.UpdatePartial(TypeRepr.of[tpe], field.valueOrAbort, name, fun.asTerm, TypeRepr.of[from])

        case '{ WriterDescription.BuilderOperation.UpdatePartialFromRoot.apply[tpe]($field, $name, $fun) } =>
          WriterMacroOperation.UpdatePartialFromRoot(TypeRepr.of[tpe], field.valueOrAbort, name, fun.asTerm)

        case '{ WriterDescription.BuilderOperation.Add.apply[tpe, to]($field, $fun) } =>
          WriterMacroOperation.Add(TypeRepr.of[tpe], field, fun.asTerm, TypeRepr.of[to])
      }
  }

  protected def evalWriterConfig(configExpr: Expr[WriterDerivationConfig]): (Option[FieldStyle], Option[String]) = {
    def parseConfigExpr(
        confExpr: Expr[WriterDerivationConfig],
        fieldStyleExpr: Option[Expr[ConfigFieldStyle]],
        discriminatorExpr: Option[Expr[String]]
    ): (Option[Expr[ConfigFieldStyle]], Option[Expr[String]]) =
      confExpr match {
        case '{ WriterDerivationConfig.empty } =>
          (fieldStyleExpr, discriminatorExpr)

        case '{ WriterDerivationConfig.apply(None, None) } =>
          (fieldStyleExpr, discriminatorExpr)

        case '{ WriterDerivationConfig.apply(None, Some($discriminator)) } =>
          (fieldStyleExpr, discriminatorExpr.orElse(Some(discriminator)))

        case '{ WriterDerivationConfig.apply(Some($fieldStyle), None) } =>
          (fieldStyleExpr.orElse(Some(fieldStyle)), discriminatorExpr)

        case '{ WriterDerivationConfig.apply(Some($fieldStyle), Some($discriminator)) } =>
          (fieldStyleExpr.orElse(Some(fieldStyle)), discriminatorExpr.orElse(Some(discriminator)))

        case '{ WriterDerivationConfig.withFieldStyle($fieldStyle) } =>
          (fieldStyleExpr.orElse(Some(fieldStyle)), discriminatorExpr)

        case '{ WriterDerivationConfig.withDiscriminator($discriminator) } =>
          (fieldStyleExpr, discriminatorExpr.orElse(Some(discriminator)))

        case '{ ($config: WriterDerivationConfig).withFieldStyle($fieldStyle) } =>
          parseConfigExpr(config, fieldStyleExpr.orElse(Some(fieldStyle)), discriminatorExpr)

        case '{ ($config: WriterDerivationConfig).withDiscriminator($discriminator) } =>
          parseConfigExpr(config, fieldStyleExpr, discriminatorExpr.orElse(Some(discriminator)))

        case _ => report.errorAndAbort(s"Config parsing error. Unknown expr: ${confExpr.asTerm.show}")
      }

    val (fieldStyleExpr, discriminatorExpr) = parseConfigExpr(configExpr, None, None)

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

        case '{ ConfigFieldStyle.lowercase }      => FieldStyle.lowercase
        case '{ ConfigFieldStyle.uppercase }      => FieldStyle.uppercase
        case '{ ConfigFieldStyle.kebabcase }      => FieldStyle.kebabcase
        case '{ ConfigFieldStyle.lowerKebabcase } => FieldStyle.lowerKebabcase
        case '{ ConfigFieldStyle.upperKebabcase } => FieldStyle.upperKebabcase
        case '{ ConfigFieldStyle.snakecase }      => FieldStyle.snakecase
        case '{ ConfigFieldStyle.lowerSnakecase } => FieldStyle.lowerSnakecase
        case '{ ConfigFieldStyle.upperSnakecase } => FieldStyle.upperSnakecase

        case _ => report.errorAndAbort("fieldStyle in writer config can not be computed in compile-time")
      }
    }

    val discriminator = discriminatorExpr.map(
      _.asTerm.underlying
        .asExprOf[String]
        .value
        .getOrElse(report.errorAndAbort("discriminator in writer config can not be computed in compile-time"))
    )

    (fieldStyle, discriminator)
  }
}
