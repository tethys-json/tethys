package tethys.fs2

import com.fasterxml.jackson.core.async.ByteArrayFeeder
import com.fasterxml.jackson.core.{
  JsonFactory,
  JsonParser,
  JsonToken,
  JsonTokenId
}
import tethys.commons.Token
import tethys.commons.Token._

import scala.annotation.switch

final class Cursor private (jsonParser: JsonParser) {
  import Cursor._

  private[this] val inputFeeder: ByteArrayFeeder =
    jsonParser.getNonBlockingInputFeeder match {
      case feeder: ByteArrayFeeder => feeder
      case other =>
        throw new IllegalArgumentException(
          s"Expected ByteArrayFeeder but found: ${other.getClass.getName}"
        )
    }

  private[this] var token: Token = Token.Empty
  private[this] var scopes: List[Scope] = Nil

  def feedInput(bytes: Array[Byte]): Unit = {
    if (bytes.nonEmpty) {
      inputFeeder.feedInput(bytes, 0, bytes.length)
    }
  }

  def endOfInput(): Unit = inputFeeder.endOfInput()

  def next(): Step = {
    val nextToken = jsonParser.nextToken()

    if (nextToken == null) {
      token = Token.Empty
      Step.EndOfInput
    } else if (nextToken == JsonToken.NOT_AVAILABLE) {
      token = Token.Empty
      Step.InputEmpty
    } else {
      updateScopes(nextToken)
      token = fromToken(nextToken)
      Step.Available(token)
    }
  }

  def currentToken(): Token = token

  def scopeDepth: Int = scopes.length

  def history: List[String] = scopes.flatMap(_.segment).reverse

  def currentName: Option[String] = scopes.headOption.flatMap(_.segment)

  def fieldName(): String = jsonParser.getCurrentName

  def string(): String = jsonParser.getValueAsString

  def number(): Number = jsonParser.getNumberValue

  def boolean(): Boolean = jsonParser.getBooleanValue

  def close(): Unit = jsonParser.close()

  private def updateScopes(nextToken: JsonToken): Unit = nextToken match {
    case JsonToken.FIELD_NAME =>
      scopes match {
        case (obj: ObjectScope) :: _ =>
          obj.pendingFieldName = Some(jsonParser.getCurrentName)
        case _ => ()
      }

    case JsonToken.START_OBJECT =>
      val segment = consumePendingFieldName()
      scopes = ObjectScope(segment, None) :: scopes

    case JsonToken.START_ARRAY =>
      val segment = consumePendingFieldName()
      scopes = ArrayScope(segment) :: scopes

    case JsonToken.END_OBJECT | JsonToken.END_ARRAY =>
      scopes = scopes.tail

    case _ => ()
  }

  private def consumePendingFieldName(): Option[String] = {
    scopes match {
      case (obj: ObjectScope) :: _ =>
        val fieldName = obj.pendingFieldName
        obj.pendingFieldName = None
        fieldName
      case _ => None
    }
  }

  private def fromToken(jsonToken: JsonToken): Token =
    (jsonToken.id(): @switch) match {
      case JsonTokenId.ID_START_OBJECT => ObjectStartToken
      case JsonTokenId.ID_END_OBJECT   => ObjectEndToken
      case JsonTokenId.ID_START_ARRAY  => ArrayStartToken
      case JsonTokenId.ID_END_ARRAY    => ArrayEndToken
      case JsonTokenId.ID_FIELD_NAME   => FieldNameToken
      case JsonTokenId.ID_STRING       => StringValueToken
      case JsonTokenId.ID_NUMBER_INT   => NumberValueToken
      case JsonTokenId.ID_NUMBER_FLOAT => NumberValueToken
      case JsonTokenId.ID_TRUE         => BooleanValueToken
      case JsonTokenId.ID_FALSE        => BooleanValueToken
      case JsonTokenId.ID_NULL         => NullValueToken
      case _                           => Token.Empty
    }
}

object Cursor {
  private[this] lazy val defaultJsonFactory: JsonFactory = {
    val jsonFactory = new JsonFactory()
    jsonFactory.configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
    jsonFactory
  }

  def apply(jsonFactory: JsonFactory = defaultJsonFactory): Cursor =
    new Cursor(jsonFactory.createNonBlockingByteArrayParser())

  sealed trait Step
  object Step {
    final case class Available(token: Token) extends Step
    case object InputEmpty extends Step
    case object EndOfInput extends Step
  }

  private sealed trait Scope {
    def segment: Option[String]
  }

  private final case class ObjectScope(
      segment: Option[String],
      var pendingFieldName: Option[String]
  ) extends Scope

  private final case class ArrayScope(segment: Option[String]) extends Scope
}
