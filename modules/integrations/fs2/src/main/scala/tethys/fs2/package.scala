package tethys

import cats.effect.Sync
import cats.MonadError
import cats.syntax.flatMap._
import _root_.fs2.{Compiler, Stream}
import tethys.readers.ReaderError
import tethys.writers.tokens.TokenWriterProducer

package object fs2 {
  implicit def decoderOps[A](jsonReader: JsonReader[A]): DecoderOps[A] =
    new DecoderOps[A](jsonReader)

  implicit class JsonStreamWriterOps[F[_], A](private val stream: Stream[F, A])
      extends AnyVal {
    def asJsonArrayByteStream(
        charset: String = "UTF-8"
    )(implicit
        jsonWriter: JsonWriter[A],
        tokenWriterProducer: TokenWriterProducer
    ): Stream[F, Byte] = {
      val separator = ",".getBytes(charset)

      Stream.emit('['.toByte) ++
        stream
          .map(_.asJson.getBytes(charset))
          .intersperse(separator)
          .flatMap(bytes => Stream.emits(bytes)) ++
        Stream.emit(']'.toByte)
    }
  }

  class DecoderOps[A](private val jsonReader: JsonReader[A]) extends AnyVal {
    def decodeFromStream[F[_], G[_]](
        stream: Stream[F, Array[Byte]],
        charset: String = "UTF-8"
    )(implicit
        compiler: Compiler[F, G],
        monadError: MonadError[G, Throwable]
    ): G[A] = {
      val decoder = new StreamingDecoder[A](Cursor(), jsonReader)

      stream
        .fold(decoder) { (state, bytes) =>
          state.feed(bytes)
          state
        }
        .map(_.finish())
        .compile
        .lastOrError
        .flatMap(result => MonadError[G, Throwable].fromEither(result))
    }

    def decodeDocumentsFromStream[F[_]: Sync](
        stream: Stream[F, Byte]
    ): Stream[F, Either[ReaderError, A]] = {
      val cursor =
        Stream.bracket(Sync[F].delay(Cursor()))(c => Sync[F].delay(c.close()))

      cursor.flatMap { c =>
        val parser = new DocumentsParser[A](c, jsonReader)

        stream.chunks.noneTerminate.flatMap {
          case Some(chunk) =>
            Stream.emits(parser.feed(chunk.toArray))
          case None =>
            parser.finish() match {
              case Right(results) => Stream.emits(results)
              case Left(error)    => Stream.raiseError[F](error)
            }
        }
      }
    }

    def decodeNDocumentsFromStream[F[_]: Sync, G[_]](
        stream: Stream[F, Byte],
        number: Int
    )(implicit compiler: Compiler[F, G]): G[List[Either[ReaderError, A]]] = {
      decodeDocumentsFromStream(stream)
        .take(number.toLong.max(0L))
        .compile
        .toList
    }
  }

}
