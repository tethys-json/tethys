package tethys.derivation

import tethys.selector

sealed abstract class SimpleSealedType(@selector val __type: String)
case class CaseClass(a: Int) extends SimpleSealedType("CaseClass")
case class SimpleClass(val b: Int) extends SimpleSealedType("SimpleClass")
case object JustObject extends SimpleSealedType("JustObject")

sealed abstract class SimpleSealedTypeSub(override val __type: String) extends SimpleSealedType(__type: String)
case class SubChild(c: Int) extends SimpleSealedTypeSub("SubChild") 
