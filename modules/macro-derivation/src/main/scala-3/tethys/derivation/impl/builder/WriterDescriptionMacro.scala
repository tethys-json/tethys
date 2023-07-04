package tethys.derivation.impl.builder

import tethys.derivation.builder.{WriterBuilder, WriterDescription}

import scala.quoted.*

class WriterDescriptionMacro(val quotes: Quotes) extends WriterBuilderCommons {
  implicit val context: Quotes = quotes
  import context.reflect.*
  
  def simpleDescription[T <: Product : Type](builder: Expr[WriterBuilder[T]]): Expr[WriterDescription[T]] =
    convertWriterBuilder[T](builder)
}
