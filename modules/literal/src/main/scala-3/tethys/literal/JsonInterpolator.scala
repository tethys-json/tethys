package tethys.literal

import tethys.{JsonWriter, JsonWriterOps}
import tethys.commons.RawJson
import tethys.literal.JsonLiteralParser.HolePosition
import tethys.writers.KeyWriter
import tethys.writers.tokens.TokenWriterProducer

import scala.quoted.*

extension (inline sc: StringContext)
  inline def json(inline args: Any*): RawJson =
    ${ JsonInterpolatorMacro.impl('sc, 'args) }

private[literal] object JsonInterpolatorMacro {

  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using
      Quotes
  ): Expr[RawJson] = {
    import quotes.reflect.*

    val parts: List[String] = sc match {
      case '{ StringContext(${ Varargs(rawParts) }*) } =>
        rawParts.toList.map {
          case Expr(s: String) => s
          case other =>
            report.errorAndAbort(
              "json interpolator requires constant string parts",
              other
            )
        }
      case _ =>
        report.errorAndAbort("Unexpected StringContext shape", sc)
    }

    val argExprs: List[Expr[Any]] = args match {
      case Varargs(xs) => xs.toList
      case _ =>
        report.errorAndAbort(
          "Expected varargs for json interpolator args",
          args
        )
    }

    if (parts.length != argExprs.length + 1)
      report.errorAndAbort(
        s"Malformed StringContext: ${parts.length} parts, ${argExprs.length} args"
      )

    val badIdx = JsonLiteralParser.findHoleInString(parts)
    if (badIdx >= 0)
      report.errorAndAbort(
        "Interpolation inside a string literal is not supported — move the placeholder into a value or key position",
        argExprs(badIdx)
      )

    val template = JsonLiteralParser.assembleTemplate(parts)
    val holes = JsonLiteralParser.parse(template, argExprs.size) match {
      case Right(hs) => hs
      case Left(err) =>
        report.errorAndAbort(
          s"Invalid JSON literal (at offset ${err.offset}): ${err.message}",
          sc
        )
    }

    if (argExprs.nonEmpty && Expr.summon[TokenWriterProducer].isEmpty)
      report.errorAndAbort(
        "No implicit TokenWriterProducer in scope. Did you forget `import tethys.jackson.*`?",
        sc
      )

    val positionByIndex: Map[Int, HolePosition] =
      holes.iterator.map(h => h.index -> h.position).toMap

    val argSnippets: IndexedSeq[Expr[String]] =
      argExprs.zipWithIndex.toIndexedSeq.map { case (argE, i) =>
        positionByIndex.getOrElse(
          i,
          report
            .errorAndAbort(s"Internal: no resolved position for arg #$i", argE)
        ) match {
          case HolePosition.Value => renderValue(argE)
          case HolePosition.Key   => renderKey(argE)
        }
      }

    val pieces: List[Expr[String]] =
      parts.zipWithIndex.flatMap { case (p, i) =>
        val lit: Expr[String] = Expr(p)
        if (i < argSnippets.length) List(lit, argSnippets(i)) else List(lit)
      }

    val concat: Expr[String] = pieces match {
      case Nil       => Expr("")
      case h :: tail => tail.foldLeft(h)((acc, e) => '{ $acc + $e })
    }

    '{ RawJson($concat) }
  }

  private def renderValue(using Quotes)(arg: Expr[Any]): Expr[String] = {
    import quotes.reflect.*
    arg.asTerm.tpe.widen.asType match {
      case '[t] =>
        Expr.summon[JsonWriter[t]] match {
          case Some(w) =>
            val typedArg = arg.asExprOf[t]
            '{
              ${ typedArg }.asJsonWith($w)(using
                scala.compiletime.summonInline[TokenWriterProducer]
              )
            }
          case None =>
            report.errorAndAbort(
              s"No JsonWriter[${Type.show[t]}] in scope for interpolated value",
              arg
            )
        }
    }
  }

  private def renderKey(using Quotes)(arg: Expr[Any]): Expr[String] = {
    import quotes.reflect.*
    arg.asTerm.tpe.widen.asType match {
      case '[t] =>
        Expr.summon[KeyWriter[t]] match {
          case Some(kw) =>
            val typedArg = arg.asExprOf[t]
            '{
              ${ kw }
                .toKey(${ typedArg })
                .asJsonWith(summon[JsonWriter[String]])(using
                  scala.compiletime.summonInline[TokenWriterProducer]
                )
            }
          case None =>
            report.errorAndAbort(
              s"No KeyWriter[${Type.show[t]}] in scope for interpolated object key",
              arg
            )
        }
    }
  }
}
