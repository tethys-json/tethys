package tethys.derivation

import tethys.JsonReader

import scala.quoted.{Quotes, ToExpr}

private[derivation]
case class JsonReaderProductConfigParsed(
  defaultValues: Map[String, Any],
  fields: List[JsonReaderProductConfigParsed.Field],
  readers: Map[String, JsonReader[?]],
  requiredLabels: Set[String],
  fieldsWithoutReaders: Set[String],
  isStrict: Boolean
)


private[derivation]
object JsonReaderProductConfigParsed:
  case class Field(
    name: String,
    idx: Int,
    function: Option[Any => Any],
    dependencies: List[(String, Option[Int])],
    extractReader: Boolean
  )
      
      
