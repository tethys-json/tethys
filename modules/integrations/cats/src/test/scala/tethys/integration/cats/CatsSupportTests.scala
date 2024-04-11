package tethys.integration.cats

import cats.data._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode._
import tethys.integration.cats.instances._
import tethys.readers.ReaderError
import tethys.writers.tokens.SimpleTokenWriter._

class CatsSupportTests extends AnyFlatSpec with Matchers {
  val nev: NonEmptyVector[String] = NonEmptyVector.of("a", "b")
  val nel: NonEmptyList[Int] = NonEmptyList.of(1, 2)
  val nes: NonEmptySet[String] = NonEmptySet.of("a", "b")
  val chain: Chain[Int] = Chain.fromSeq(Seq(1, 2, 3))
  val nec: NonEmptyChain[String] = NonEmptyChain.of("a", "b", "c")

  behavior of "CatsWriters"
  it should "work with non empty" in {
    nev.asTokenList shouldBe arr("a", "b")
    nel.asTokenList shouldBe arr(1, 2)
    nes.asTokenList shouldBe arr("a", "b")
    chain.asTokenList shouldBe arr(1, 2, 3)
    nec.asTokenList shouldBe arr("a", "b", "c")
  }

  behavior of "CatsReaders"
  it should "work with non empty" in {
    nev shouldBe arr("a", "b").tokensAs[NonEmptyVector[String]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptyVector[String]])

    nel shouldBe arr(1, 2).tokensAs[NonEmptyList[Int]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptyList[Int]])

    nes shouldBe arr("a", "b").tokensAs[NonEmptySet[String]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptySet[String]])

    chain shouldBe arr(1, 2, 3).tokensAs[Chain[Int]]

    nec shouldBe arr("a", "b", "c").tokensAs[NonEmptyChain[String]]
    assertThrows[ReaderError](Nil.tokensAs[NonEmptyChain[String]])

  }
}
