package tethys.derivation.impl.builder

import tethys.derivation.builder.{WriterBuilder, WriterDescription}

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 23.04.17.
  */
class WriterDescriptorMacro(val c: blackbox.Context) extends WriterBuilderCommons {

  import c.universe._

  def simpleDescription[A: WeakTypeTag](builder: Expr[WriterBuilder[A]]): Expr[WriterDescription[A]] = {
    convertWriterBuilder[A](builder)
  }

}
