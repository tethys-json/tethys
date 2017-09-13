package tethys.derivation

import org.scalatest.{FlatSpec, Matchers}
import tethys.core.writers.syntax._
import tethys.derivation.ADTWithType._
import tethys.derivation.ADTWithWrongType._
import tethys.derivation.auto._

class AutoDerivationTest extends FlatSpec with Matchers {

  it should "auto derive writer for recursive type" in {
    RecursiveType(1, Seq(RecursiveType(2))).asJson.shouldBe(
      """{"a":1,"children":[{"a":2,"children":[]}]}"""
    )
  }

  it should "auto derive writer for A => B => A cycle" in {
    ComplexRecursionA(1, Some(ComplexRecursionB(2, ComplexRecursionA(3, None)))).asJson.shouldBe(
      """{"a":1,"b":{"b":2,"a":{"a":3}}}"""
    )
  }

  it should "auto derive writer for sealed cyclic trait with type parameter" in {
    val t: ADTWithType[Int] = ADTWithTypeB[Int](1, ADTWithTypeA[Int](2))

    t.asJson.shouldBe(
      """{"a":1,"b":{"a":2}}"""
    )
  }

  it should "not auto derive writer for sealed cyclic trait with type parameter if one of subclasses has additional type" in {
    val t: ADTWithWrongType[Int] = ADTWithWrongTypeB[Int, String](1, ADTWithWrongTypeA[String]("2"))

    "t.asJson" shouldNot compile
  }
}
