package tethys.literal

import tethys.commons.RawJson
import tethys.literal.JsonLiteralParser.HolePosition

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

final class JsonInterpolator(private val sc: StringContext) extends AnyVal {
  def json(args: Any*): RawJson = macro JsonInterpolatorMacro.impl
}

object JsonInterpolator {
  implicit def toJsonInterpolator(sc: StringContext): JsonInterpolator =
    new JsonInterpolator(sc)
}

private[literal] object JsonInterpolatorMacro {
  def impl(c: blackbox.Context)(args: c.Expr[Any]*): c.Expr[RawJson] = {
    import c.universe._

    val parts: List[String] = c.prefix.tree match {
      case Apply(_, List(Apply(_, rawParts))) =>
        rawParts.map {
          case Literal(Constant(s: String)) => s
          case other =>
            c.abort(
              other.pos,
              "json interpolator requires constant string parts"
            )
        }
      case other =>
        c.abort(
          other.pos,
          "Unexpected prefix shape for json interpolator"
        )
    }

    val argTrees: List[Tree] = args.iterator.map(_.tree).toList

    if (parts.length != argTrees.length + 1)
      c.abort(
        c.enclosingPosition,
        s"Malformed StringContext: ${parts.length} parts, ${argTrees.length} args"
      )

    val badIdx = JsonLiteralParser.findHoleInString(parts)
    if (badIdx >= 0)
      c.abort(
        argTrees(badIdx).pos,
        "Interpolation inside a string literal is not supported — move the placeholder into a value or key position"
      )

    val template = JsonLiteralParser.assembleTemplate(parts)
    val holes = JsonLiteralParser.parse(template, argTrees.size) match {
      case Right(hs) => hs
      case Left(err) =>
        c.abort(
          c.enclosingPosition,
          s"Invalid JSON literal (at offset ${err.offset}): ${err.message}"
        )
    }

    val producerTpe = c.weakTypeOf[tethys.writers.tokens.TokenWriterProducer]
    val producerTree =
      if (argTrees.isEmpty) EmptyTree
      else {
        val found = c.inferImplicitValue(producerTpe)
        if (found.isEmpty)
          c.abort(
            c.enclosingPosition,
            "No implicit TokenWriterProducer in scope. Did you forget `import tethys.jackson._`?"
          )
        found
      }

    val positionByIndex: Map[Int, HolePosition] =
      holes.iterator.map(h => h.index -> h.position).toMap

    def renderValue(arg: Tree): Tree = {
      val tpe       = c.typecheck(arg.duplicate, silent = false).tpe.widen
      val writerTpe = appliedType(weakTypeOf[tethys.JsonWriter[_]].typeConstructor, tpe)
      val writer    = c.inferImplicitValue(writerTpe)
      if (writer.isEmpty)
        c.abort(arg.pos, s"No JsonWriter[$tpe] in scope for interpolated value")
      q"new _root_.tethys.JsonWriterOps[$tpe]($arg).asJsonWith($writer)($producerTree)"
    }

    def renderKey(arg: Tree): Tree = {
      val tpe          = c.typecheck(arg.duplicate, silent = false).tpe.widen
      val keyWriterTpe = appliedType(weakTypeOf[tethys.writers.KeyWriter[_]].typeConstructor, tpe)
      val keyWriter    = c.inferImplicitValue(keyWriterTpe)
      if (keyWriter.isEmpty)
        c.abort(arg.pos, s"No KeyWriter[$tpe] in scope for interpolated object key")
      val stringWriter = c.inferImplicitValue(typeOf[tethys.JsonWriter[String]])
      if (stringWriter.isEmpty)
        c.abort(c.enclosingPosition, "No JsonWriter[String] in scope (should be provided by tethys core)")
      q"new _root_.tethys.JsonWriterOps[_root_.scala.Predef.String]($keyWriter.toKey($arg)).asJsonWith($stringWriter)($producerTree)"
    }

    val argSnippets: IndexedSeq[Tree] =
      argTrees.zipWithIndex.toIndexedSeq.map { case (argT, i) =>
        positionByIndex.getOrElse(
          i,
          c.abort(argT.pos, s"Internal: no resolved position for arg #$i")
        ) match {
          case HolePosition.Value => renderValue(argT)
          case HolePosition.Key   => renderKey(argT)
        }
      }

    val pieces: List[Tree] =
      parts.zipWithIndex.flatMap { case (p, i) =>
        val lit: Tree = Literal(Constant(p))
        if (i < argSnippets.length) List(lit, argSnippets(i)) else List(lit)
      }

    val concat: Tree = pieces match {
      case Nil       => Literal(Constant(""))
      case h :: tail => tail.foldLeft(h)((acc, e) => q"$acc + $e")
    }

    c.Expr[RawJson](q"_root_.tethys.commons.RawJson($concat)")
  }
}
