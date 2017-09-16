package tethys.readers

import scala.reflect.ClassTag
import scala.util.control.NonFatal

sealed class ReaderError protected(message: String, cause: Throwable, field: String) extends Exception(message, cause)

final class WrongTypeError(message: String, field: String) extends ReaderError(
  message = message,
  cause = null,
  field = field
)

final class WrongJsonError(field: String) extends ReaderError(
  message = s"Json is not properly formatted: '$field'",
  cause = null,
  field = field
)

object ReaderError {
  def wrongType[A](implicit fieldName: FieldName, classTag: ClassTag[A]): Nothing = throw {
    val name = fieldName.value()
    new WrongTypeError(
      message = s"Can't parse ${classTag.runtimeClass.getName} from field '$name'",
      field = name
    )
  }

  def wrongJson(implicit fieldName: FieldName): Nothing = throw new WrongJsonError(fieldName.value())

  def catchNonFatal[A](fun: => A)(implicit fieldName: FieldName): Either[ReaderError, A] = {
    try Right(fun) catch {
      case err: ReaderError => Left(err)
      case NonFatal(e) => Left(new ReaderError(e.getMessage, e, fieldName.value()))
    }
  }
}