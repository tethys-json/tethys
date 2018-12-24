package tethys.derivation.impl.builder

import tethys.derivation.builder.{ReaderBuilder, ReaderDescription}

import scala.reflect.macros.blackbox

class ReaderDescriptionMacro(val c: blackbox.Context) extends ReaderDescriptionCommons {
  import c.universe._

  def readerDescription[A: WeakTypeTag](builder: Expr[ReaderBuilder[A]]): Expr[ReaderDescription[A]] = {
    convertReaderBuilder[A](builder)
  }
}
