package tethys

package object derivation {
  case class SimpleType(i: Int, s: String, d: Double)
  case class SimpleTypeWithAny(i: Int, s: String, d: Double, any: Any)

  case class JsonTreeTestData(a: Int, b: Boolean, c: C)
  case class C(d: D)
  case class D(a: Int)


  case class RecursiveType(a: Int, children: Seq[RecursiveType] = Seq.empty)

  case class ComplexRecursionA(a: Int, b: Option[ComplexRecursionB])
  case class ComplexRecursionB(b: Int, a: ComplexRecursionA)

  trait JsonComplexTestData
  case class JsonComplexTestDataImpl1(a: Int) extends JsonComplexTestData
  case class JsonComplexTestDataImpl2(b: String) extends JsonComplexTestData

  case class SeqMaster1(a: Seq[SeqMaster2])
  case class SeqMaster2(a: Seq[SeqMaster3])
  case class SeqMaster3(a: Seq[SeqMaster4])
  case class SeqMaster4(a: Seq[Int])

  case class CamelCaseNames(someParam: Int, IDParam: Int, simple: Int)
}
