package tethys.fs2

import tethys.JsonReader
import tethys.fs2.Cursor.Step
import tethys.fs2.JsonStreamSupport.{
  Collector,
  decode,
  isImmediateArrayElementStart,
  rootError
}
import tethys.readers.ReaderError

private final class DocumentsParser[A](
    cursor: Cursor,
    jsonReader: JsonReader[A]
) {
  private[this] var collector: Option[Collector] = None

  def feed(bytes: Array[Byte]): Vector[Either[ReaderError, A]] = {
    cursor.feedInput(bytes)
    drain()
  }

  def finish(): Either[ReaderError, Vector[Either[ReaderError, A]]] = {
    cursor.endOfInput()
    val results = drain()

    if (collector.nonEmpty || cursor.scopeDepth != 0) {
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
          collector match {
            case Some(currentCollector) =>
              currentCollector.consume(cursor)
              if (currentCollector.isComplete) {
                results += decode(currentCollector, jsonReader)
                collector = None
              }

            case None =>
              if (isImmediateArrayElementStart(cursor, 0)) {
                val currentCollector = Collector.start(cursor)
                collector = Some(currentCollector)
                if (currentCollector.isComplete) {
                  results += decode(currentCollector, jsonReader)
                  collector = None
                }
              }
          }

        case Step.InputEmpty | Step.EndOfInput =>
          continue = false
      }
    }

    results.result()
  }
}
