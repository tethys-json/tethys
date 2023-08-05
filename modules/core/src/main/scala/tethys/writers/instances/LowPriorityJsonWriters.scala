package tethys.writers.instances

import tethys._
import tethys.commons.LowPriorityInstance

private[tethys] trait LowPriorityJsonWriters {
  implicit final def lowPriorityWriter[A](implicit
      lowPriorityInstance: LowPriorityInstance[JsonObjectWriter[A]]
  ): JsonWriter[A] = {
    lowPriorityInstance.instance
  }
}
