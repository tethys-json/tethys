package tethys.derivation.impl

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 22.04.17.
  */
trait BaseMacroDefinitions {
  val c: blackbox.Context
  import c.universe._

  lazy val tethysPack = q"tethys"

  lazy val writersPack = q"$tethysPack.writers"

  lazy val readersPack = q"$tethysPack.readers"

  lazy val macroPack = q"$tethysPack.derivation.impl"

  lazy val buildersPack = q"$tethysPack.derivation.builder"

}
