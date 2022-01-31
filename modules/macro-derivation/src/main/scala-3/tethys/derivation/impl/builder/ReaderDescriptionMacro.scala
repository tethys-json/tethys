package tethys.derivation.impl.builder

import scala.quoted.{Expr, Quotes, Type}

import tethys.derivation.builder.{ReaderBuilder, ReaderDescription}

class ReaderDescriptionMacro(val quotes: Quotes) extends ReaderBuilderCommons {
  implicit val context: Quotes = quotes
  import context.reflect.*

  def simpleDescription[T <: Product : Type](builder: Expr[ReaderBuilder[T]]): Expr[ReaderDescription[T]] =
    convertReaderBuilder[T](builder)
}
