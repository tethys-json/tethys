package tethys.readers

import scala.util.control.NonFatal

final class ReaderError private (
    message: String,
    cause: Throwable,
    field: String
) extends Exception(message, cause)

object ReaderError {
  def wrongJson(reason: String, cause: Throwable = null)(implicit
      fieldName: FieldName
  ): Nothing = {
    val field = fieldName.value()
    throw new ReaderError(
      message = errorMessage(reason, field),
      cause = cause,
      field = field
    )
  }

  def catchNonFatal[A](
      fun: => A
  )(implicit fieldName: FieldName): Either[ReaderError, A] = {
    try Right(fun)
    catch {
      case err: ReaderError => Left(err)
      case NonFatal(e) =>
        Left(
          new ReaderError(
            message = e.getMessage,
            cause = e,
            field = fieldName.value()
          )
        )
    }
  }

  final case class Details(
      reason: String,
      cause: Throwable = null
  ) {
    def toError(implicit fieldName: FieldName): ReaderError = {
      val field = fieldName.value()
      new ReaderError(errorMessage(reason, field), cause, field)
    }
  }

  private def errorMessage(reason: String, field: String): String =
    s"Illegal json at '$field': $reason"
}
