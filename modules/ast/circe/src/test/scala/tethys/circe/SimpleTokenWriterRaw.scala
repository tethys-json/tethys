package tethys.circe

import tethys.commons.TokenNode
import tethys.writers.tokens.SimpleTokenWriter

object SimpleTokenWriterRaw {
  implicit final class SimpleTokenWriterRawOps[A](val a: A) extends AnyVal {
    import tethys.*
    import tethys.jackson.*

    def asTokenList(implicit jsonWriter: JsonWriter[A]): List[TokenNode] = {
      val tokenWriter = new SimpleTokenWriter().withRawJsonSupport
      a.writeJson(tokenWriter)
      tokenWriter.tokens.toList
    }
  }
}
