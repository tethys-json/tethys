package tethys

package object derivation {
  case class JsonTreeTestData(a: Int, b: Boolean, c: C)
  case class C(d: D)
  case class D(a: Int)


  case class RecursiveType(a: Int, children: Seq[RecursiveType] = Seq.empty)

  case class ComplexRecursionA(a: Int, b: Option[ComplexRecursionB])
  case class ComplexRecursionB(b: Int, a: ComplexRecursionA)

  trait JsonComplexTestData
  case class JsonComplexTestDataImpl1(a: Int) extends JsonComplexTestData
  case class JsonComplexTestDataImpl2(b: String) extends JsonComplexTestData
}
