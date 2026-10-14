package tethys.fs2

import cats.effect.Sync
import fs2.Stream
import tethys.JsonReader
import tethys.fs2.Cursor.Step
import tethys.fs2.JsonStreamSupport._
import tethys.readers.ReaderError

object Parse {
  def oneDocument: OneDocument = OneDocument(Nil)

  def oneDocument(fieldName: String): OneDocument = OneDocument(
    fieldName :: Nil
  )

  final case class OneDocument(path: List[String]) {
    def inField(fieldName: String): OneDocument = copy(path = path :+ fieldName)

    def everyElementAs[T: JsonReader]: OneDocumentDecoderApplied[T] =
      OneDocumentDecoderApplied(path, JsonReader[T])
  }

  final case class OneDocumentDecoderApplied[T](
      path: List[String],
      decoder: JsonReader[T]
  ) {
    def toFs2Stream[F[_]: Sync](
        initialStream: Stream[F, Byte]
    ): Stream[F, Either[ReaderError, T]] = {
      val cursor =
        Stream.bracket(Sync[F].delay(Cursor()))(c => Sync[F].delay(c.close()))

      cursor.flatMap { c =>
        val parser = new ArrayElementsParser[T](c, path, decoder)

        initialStream.chunks.noneTerminate.flatMap {
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
  }
}

private final class ArrayElementsParser[A](
    cursor: Cursor,
    path: List[String],
    jsonReader: JsonReader[A]
) {
  private[this] var collector: Option[Collector] = None
  private[this] var targetArrayDepths: List[Int] = Nil
  private[this] var hasSeenTokens: Boolean = false

  def feed(bytes: Array[Byte]): Vector[Either[ReaderError, A]] = {
    cursor.feedInput(bytes)
    drain()
  }

  def finish(): Either[ReaderError, Vector[Either[ReaderError, A]]] = {
    cursor.endOfInput()
    val results = drain()

    if (
      !hasSeenTokens || collector.nonEmpty || targetArrayDepths.nonEmpty || cursor.scopeDepth != 0
    ) {
      Left(rootError("Unexpected end of input"))
    } else {
      Right(results)
    }
  }

  private def drain(): Vector[Either[ReaderError, A]] = {
    val results = Vector.newBuilder[Either[ReaderError, A]]
    var continue = true

    while (continue) {
      cursor.next() match {
        case Step.Available(_) =>
          hasSeenTokens = true

          collector match {
            case Some(currentCollector) =>
              currentCollector.consume(cursor)
              if (currentCollector.isComplete) {
                results += decode(currentCollector, jsonReader)
                collector = None
              }

            case None =>
              val token = cursor.currentToken()

              if (matchesArrayPath(cursor, path)) {
                targetArrayDepths = cursor.scopeDepth :: targetArrayDepths
              } else {
                targetArrayDepths.headOption.foreach { targetArrayDepth =>
                  if (isImmediateArrayElementStart(cursor, targetArrayDepth)) {
                    val currentCollector = Collector.start(cursor)
                    collector = Some(currentCollector)
                    if (currentCollector.isComplete) {
                      results += decode(currentCollector, jsonReader)
                      collector = None
                    }
                  }
                }
              }

              if (
                collector.isEmpty && token.isArrayEnd && targetArrayDepths.headOption
                  .contains(
                    cursor.scopeDepth + 1
                  )
              ) {
                targetArrayDepths = targetArrayDepths.tail
              }
          }

        case Step.InputEmpty | Step.EndOfInput =>
          continue = false
      }
    }

    results.result()
  }
}
