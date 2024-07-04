package tethys.derivation

import tethys.JsonReader

import scala.quoted.{Quotes, ToExpr}

private[derivation] case class ReaderBuilderParsed(
    defaultValues: Map[String, Any],
    fields: List[ReaderBuilderParsed.Field],
    readers: Map[String, JsonReader[?]],
    requiredLabels: Set[String],
    fieldsWithoutReaders: Set[String],
    isStrict: Boolean
)

private[derivation] object ReaderBuilderParsed:
  case class Field(
      name: String,
      idx: Int,
      function: Option[Any => Any],
      dependencies: List[(String, Option[Int])],
      extractReader: Boolean
  )
