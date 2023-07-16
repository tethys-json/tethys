package tethys.refined

import eu.timepit.refined.api.{RefType, Validate}
import tethys.readers.{FieldName, KeyReader, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonReader, JsonWriter}

trait TethysRefinedInstances {
  implicit final def RefinedJsonWriter[T: JsonWriter, P, F[_, _]: RefType]
      : JsonWriter[F[T, P]] =
    JsonWriter[T].contramap(RefType[F].unwrap)

  implicit final def RefinedJsonReader[T: JsonReader, P, F[_, _]: RefType](
      implicit validate: Validate[T, P]
  ): JsonReader[F[T, P]] =
    new JsonReader[F[T, P]] {
      override def read(it: TokenIterator)(implicit
          fieldName: FieldName
      ): F[T, P] =
        fromEither(RefType[F].refine(JsonReader[T].read(it)))
    }

  implicit final def RefinedKeyReader[T, P, F[_, _]: RefType](implicit
      reader: KeyReader[T],
      validate: Validate[T, P]
  ): KeyReader[F[T, P]] = new KeyReader[F[T, P]] {
    override def read(s: String)(implicit fieldName: FieldName): F[T, P] =
      fromEither(RefType[F].refine(reader.read(s)))
  }

  private def fromEither[A](
      either: Either[String, A]
  )(implicit fieldName: FieldName): A =
    either match {
      case Right(value) => value
      case Left(err)    => ReaderError.wrongJson(s"Refined error: $err")
    }
}
