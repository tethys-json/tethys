package tethys.derivation.builder

import scala.annotation.compileTimeOnly
import scala.language.implicitConversions

sealed trait ReaderField[A]

object ReaderField {
  implicit class ReaderFieldStringOps(val s: String) extends AnyVal {

    @compileTimeOnly("ReaderFieldOps.as should be defined in describe block")
    def as[A]: ReaderField[A] = throw new NotDescribedException
  }

  implicit class ReaderFieldSymbolOps(val s: Symbol) extends AnyVal {

    @compileTimeOnly("ReaderFieldOps.as should be defined in describe block")
    def as[A]: ReaderField[A] = throw new NotDescribedException
  }
}