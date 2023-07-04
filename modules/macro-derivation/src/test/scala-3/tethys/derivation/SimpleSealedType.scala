package tethys.derivation

sealed abstract class SimpleSealedType
case class CaseClass(a: Int) extends SimpleSealedType
class SimpleClass(val b: Int) extends SimpleSealedType
object JustObject extends SimpleSealedType

sealed abstract class SimpleSealedTypeSub extends SimpleSealedType
case class SubChild(c: Int) extends SimpleSealedTypeSub
