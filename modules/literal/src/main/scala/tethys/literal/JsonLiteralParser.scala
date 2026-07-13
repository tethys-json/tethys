package tethys.literal

import com.fasterxml.jackson.core.{JsonFactory, JsonParseException, JsonToken}

import scala.collection.mutable.ListBuffer

private[literal] object JsonLiteralParser {

  sealed trait HolePosition
  object HolePosition {
    case object Value extends HolePosition
    case object Key extends HolePosition
  }

  final case class Hole(index: Int, position: HolePosition)
  final case class ParseError(message: String, offset: Int)

  final case class Template(
      text: String,
      markerPattern: scala.util.matching.Regex
  )

  def findHoleInString(parts: List[String]): Int = {
    var i = 0
    while (i < parts.length - 1) {
      var inString = false
      var escaped = false
      val s = parts(i)
      var j = 0
      while (j < s.length) {
        val c = s(j)
        if (escaped) escaped = false
        else if (c == '\\') escaped = true
        else if (c == '"') inString = !inString
        j += 1
      }
      if (inString) return i
      i += 1
    }
    -1
  }

  def assembleTemplate(parts: List[String]): Template = {
    val uuid = java.util.UUID.randomUUID().toString.replace("-", "")
    val prefix = s"__TETHYS_HOLE_${uuid}_"
    val suffix = "__"
    val sb = new StringBuilder
    var i = 0
    while (i < parts.length) {
      sb.append(parts(i))
      if (i < parts.length - 1) {
        sb.append('"')
        sb.append(prefix)
        sb.append(i)
        sb.append(suffix)
        sb.append('"')
      }
      i += 1
    }
    val pattern =
      (java.util.regex.Pattern.quote(
        prefix
      ) + "(\\d+)" + java.util.regex.Pattern.quote(suffix)).r
    Template(sb.toString, pattern)
  }

  private val factory = new JsonFactory()

  def parse(tpl: Template, holeCount: Int): Either[ParseError, List[Hole]] = {
    val template = tpl.text
    val markerPattern = tpl.markerPattern
    val parser = factory.createParser(template)
    val holes = ListBuffer.empty[Hole]
    try {
      var sawToken = false
      var t: JsonToken = parser.nextToken()
      while (t != null) {
        sawToken = true
        t match {
          case JsonToken.FIELD_NAME =>
            val name = parser.getCurrentName
            markerPattern.findFirstMatchIn(name) match {
              case Some(m) if m.matched == name =>
                holes += Hole(m.group(1).toInt, HolePosition.Key)
              case _ =>
            }
          case JsonToken.VALUE_STRING =>
            val text = parser.getText
            markerPattern.findFirstMatchIn(text) match {
              case Some(m) if m.matched == text =>
                holes += Hole(m.group(1).toInt, HolePosition.Value)
              case _ =>
            }
          case _ =>
        }
        t = parser.nextToken()
      }
      if (!sawToken)
        Left(ParseError("Unexpected end of input, expected a value", 0))
      else if (holes.size != holeCount)
        Left(
          ParseError(
            s"Internal: parsed ${holes.size} holes, expected $holeCount",
            0
          )
        )
      else
        Right(holes.toList)
    } catch {
      case e: JsonParseException =>
        val raw = Option(e.getOriginalMessage).getOrElse(e.getMessage)
        Left(ParseError(raw, e.getLocation.getCharOffset.toInt))
    } finally {
      parser.close()
    }
  }
}
