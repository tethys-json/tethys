package tethys.readers.instances

import tethys.JsonReader
import tethys.commons.LowPriorityInstance

private[tethys] trait LowPriorityJsonReaders {
  implicit final def lowPriorityReader[A](implicit
      lowPriorityInstance: LowPriorityInstance[JsonReader[A]]
  ): JsonReader[A] = {
    lowPriorityInstance.instance
  }
}
