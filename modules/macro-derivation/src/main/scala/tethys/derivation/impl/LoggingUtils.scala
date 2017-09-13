package tethys.derivation.impl

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 23.04.17.
  */
trait LoggingUtils {
  val c: blackbox.Context

  def info(msg: => String, force: Boolean = false): Unit = c.info(c.enclosingPosition, msg, force)
  def warn(msg: String): Unit = c.warning(c.enclosingPosition, msg)
  def error(msg: String): Unit = c.error(c.enclosingPosition, msg)
  def abort(msg: String): Nothing = c.abort(c.enclosingPosition, msg)
}
