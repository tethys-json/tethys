package tethys.derivation

import tethys.selector

sealed abstract class SimpleSealedType
case class CaseClass(a: Int) extends SimpleSealedType
case class SimpleClass(val b: Int) extends SimpleSealedType
case object JustObject extends SimpleSealedType

sealed abstract class SimpleSealedTypeSub extends SimpleSealedType
case class SubChild(c: Int) extends SimpleSealedTypeSub
