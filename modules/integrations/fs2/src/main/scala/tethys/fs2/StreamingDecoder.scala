package tethys.fs2

import tethys.JsonReader
import tethys.fs2.Cursor.Step
import tethys.fs2.JsonStreamSupport.{Collector, decode, rootError}
import tethys.readers.ReaderError

private final class StreamingDecoder[A](
    cursor: Cursor,
    jsonReader: JsonReader[A]
) {
  private[this] var collector: Option[Collector] = None
  private[this] var result: Option[Either[ReaderError, A]] = None
  private[this] var closed: Boolean = false

  def feed(bytes: Array[Byte]): Unit = {
    if (result.isEmpty) {
      cursor.feedInput(bytes)
      drain()
    }
  }

  def finish(): Either[ReaderError, A] = {
    try {
      result.getOrElse {
        cursor.endOfInput()
        drain()
        result.getOrElse(Left(rootError("Unexpected end of input")))
      }
    } finally {
      closeIfNeeded()
    }
  }

  private def drain(): Unit = {
    var continue = true

    while (continue && result.isEmpty) {
      cursor.next() match {
        case Step.Available(_) =>
          collector match {
            case Some(currentCollector) =>
              currentCollector.consume(cursor)
            case None =>
              collector = Some(Collector.start(cursor))
          }

          collector.foreach { currentCollector =>
            if (currentCollector.isComplete) {
              result = Some(decode(currentCollector, jsonReader))
              collector = None
            }
          }

        case Step.InputEmpty | Step.EndOfInput =>
          continue = false
      }
    }
  }

  private def closeIfNeeded(): Unit = {
    if (!closed) {
      closed = true
      cursor.close()
    }
  }
}
