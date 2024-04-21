package tethys.derivation


private[tethys]
object Defaults:
  inline def collectFrom[T]: Map[Int, Any] = ${ DefaultsMacro.collect[T] }


private[derivation]
object DefaultsMacro:
  import scala.quoted.*

  def collect[T: Type](using quotes: Quotes): Expr[Map[Int, Any]] =
    import quotes.reflect.*
    val typeSymbol = TypeRepr.of[T].typeSymbol

    val res = typeSymbol.caseFields.zipWithIndex.flatMap {
      case (sym, idx) if sym.flags.is(Flags.HasDefault) =>
        val defaultValueMethodSym =
          typeSymbol.companionClass
            .declaredMethod(s"$$lessinit$$greater$$default$$${idx + 1}")
            .headOption
            .getOrElse(report.errorAndAbort(s"Error while extracting default value for field '${sym.name}'"))

        Some(Expr.ofTuple(Expr(idx) -> Ref(typeSymbol.companionModule).select(defaultValueMethodSym).asExprOf[Any]))
      case _ =>
        None
    }

    '{ Map(${ Varargs(res) }: _*) }

