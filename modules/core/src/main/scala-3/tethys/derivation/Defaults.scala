package tethys.derivation


private[tethys]
object Defaults:
  inline def collectFrom[T]: Map[Int, Any] = ${ DefaultsMacro.collect[T] }


private[derivation]
object DefaultsMacro:
  import scala.quoted.*

  def collect[T: Type](using

      Quotes
  ): Expr[Map[Int, Any]] =
    import quotes.reflect._

    val tpe = TypeRepr.of[T].typeSymbol
    val terms = tpe.primaryConstructor.paramSymss.flatten
      .filter(_.isValDef)
      .zipWithIndex
      .flatMap { case (field, idx) =>
        val defaultMethodName = s"$$lessinit$$greater$$default$$${idx + 1}"
        tpe.companionClass
          .declaredMethod(defaultMethodName)
          .headOption
          .map { defaultMethod =>
            val callDefault = {
            val base = Ident(tpe.companionModule.termRef).select(defaultMethod)
            val tParams = defaultMethod.paramSymss.headOption.filter(_.forall(_.isType))
              tParams match
                case Some(tParams) => TypeApply(base, tParams.map(TypeTree.ref))
                case _             => base
            }

            defaultMethod.tree match {
              case tree: DefDef => tree.rhs.getOrElse(callDefault)
              case _            => callDefault
            }
          }
          .map(x => Expr.ofTuple(Expr(idx) -> x.asExprOf[Any]))
      }

    '{ Map(${ Varargs(terms) }: _*) }