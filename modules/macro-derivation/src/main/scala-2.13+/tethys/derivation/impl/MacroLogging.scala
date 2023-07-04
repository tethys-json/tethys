package tethys.derivation.impl

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object MacroLogging {
  def logCode[A](tree: A): A = macro MacroLoggingImpl.logCode
  def logTree[A](tree: A): A = macro MacroLoggingImpl.logTree

  private class MacroLoggingImpl(val c: blackbox.Context) {
    import c.universe._

    def logCode(tree: Tree): Tree = {
      c.info(c.enclosingPosition, show(tree), force = false)
      tree
    }

    def logTree(tree: Tree): Tree = {
      c.info(c.enclosingPosition, showRaw(tree), force = false)
      tree
    }
  }
}
