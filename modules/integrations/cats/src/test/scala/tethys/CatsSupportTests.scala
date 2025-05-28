package tethys

import cats.data._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.catsInstances._
import tethys.commons.TokenNode._
import tethys.readers.ReaderError
import tethys.writers.tokens.SimpleTokenWriter._

class CatsSupportTests extends AnyFlatSpec with Matchers {
  val nev: NonEmptyVector[String] = NonEmptyVector.of("a", "b")
  val nel: NonEmptyList[Int] = NonEmptyList.of(1, 2)
  val nes: NonEmptySet[Int] = NonEmptySet.of(1, 2, 3, 4)
  val chain: Chain[Int] = Chain.fromIterableOnce(Seq(1, 2, 3))
  val nec: NonEmptyChain[String] = NonEmptyChain.of("a", "b", "c")

  behavior of "CatsWriters"
  it should "write non-empty" in {
    nev.asTokenList shouldBe arr("a", "b")
    nel.asTokenList shouldBe arr(1, 2)
    nes.asTokenList shouldBe arr(1, 2, 3, 4)
    chain.asTokenList shouldBe arr(1, 2, 3)
    nec.asTokenList shouldBe arr("a", "b", "c")
  }

  behavior of "CatsReaders"
  it should "read non-empty" in {
    nev shouldBe arr("a", "b").tokensAs[NonEmptyVector[String]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptyVector[String]])

    nel shouldBe arr(1, 2).tokensAs[NonEmptyList[Int]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptyList[Int]])

    nes shouldBe arr(1, 2, 3, 4).tokensAs[NonEmptySet[Int]]
    nes shouldBe arr(4, 4, 1, 3, 3, 2).tokensAs[NonEmptySet[Int]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptySet[Int]])

    chain shouldBe arr(1, 2, 3).tokensAs[Chain[Int]]

    nec shouldBe arr("a", "b", "c").tokensAs[NonEmptyChain[String]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptyChain[String]])
  }
}
