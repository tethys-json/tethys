package tethys.refined

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric._
import eu.timepit.refined.string.IPv4
import eu.timepit.refined.types.numeric._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode.{value => token, _}
import tethys.readers.ReaderError
import tethys.writers.tokens.SimpleTokenWriter._

class RefinedSupportTest extends AnyFlatSpec with Matchers {
  val posInt: PosInt = refineV[Positive].unsafeFrom(5)
  val nonNegInt: NonNegInt = refineV[NonNegative].unsafeFrom(0)
  val negInt: NegInt = refineV[Negative].unsafeFrom(-5)

  val posLong: PosLong = refineV[Positive].unsafeFrom(5L)
  val nonNegLong: NonNegLong = refineV[NonNegative].unsafeFrom(0L)
  val negLong: NegLong = refineV[Negative].unsafeFrom(-5L)

  val posDouble: PosDouble = refineV[Positive].unsafeFrom(5.0)
  val nonNegDouble: NonNegDouble = refineV[NonNegative].unsafeFrom(0.0)
  val negDouble: NegDouble = refineV[Negative].unsafeFrom(-5.0)
  val nonNaNDouble: NonNaNDouble = refineV[NonNaN].unsafeFrom(5.0)

  val posFloat: PosFloat = refineV[Positive].unsafeFrom(5.0f)
  val nonNegFloat: NonNegFloat = refineV[NonNegative].unsafeFrom(0.0f)
  val negFloat: NegFloat = refineV[Negative].unsafeFrom(-5.0f)
  val nonNaNFloat: NonNaNFloat = refineV[NonNaN].unsafeFrom(5.0f)

  val ipV4: String Refined IPv4 = refineV[IPv4].unsafeFrom("192.168.0.1")

  val nel: List[String] Refined NonEmpty =
    refineV[NonEmpty].unsafeFrom(List("a", "b"))

  behavior of "RefinedJsonWriter"

  it should "work with numerics" in {
    posInt.asTokenList shouldBe token(5)
    nonNegInt.asTokenList shouldBe token(0)
    negInt.asTokenList shouldBe token(-5)

    posLong.asTokenList shouldBe token(5L)
    nonNegLong.asTokenList shouldBe token(0L)
    negLong.asTokenList shouldBe token(-5L)

    posDouble.asTokenList shouldBe token(5.0)
    nonNegDouble.asTokenList shouldBe token(0.0)
    negDouble.asTokenList shouldBe token(-5.0)
    nonNaNDouble.asTokenList shouldBe token(5.0)

    posFloat.asTokenList shouldBe token(5.0f)
    nonNegFloat.asTokenList shouldBe token(0.0f)
    negFloat.asTokenList shouldBe token(-5.0f)
    nonNaNFloat.asTokenList shouldBe token(5.0f)
  }

  it should "work with strings" in {
    ipV4.asTokenList shouldBe token("192.168.0.1")
  }

  it should "work with collections" in {
    nel.asTokenList shouldBe arr("a", "b")
  }

  behavior of "RefinedJsonReader"

  it should "work with numerics" in {
    // Int
    token(5).tokensAs[PosInt] shouldBe posInt
    assertThrows[ReaderError](token(0).tokensAs[PosInt])

    token(0).tokensAs[NonNegInt] shouldBe nonNegInt
    assertThrows[ReaderError](token(-5).tokensAs[NonNegInt])

    token(-5).tokensAs[NegInt] shouldBe negInt
    assertThrows[ReaderError](token(5).tokensAs[NegInt])

    // Long
    token(5L).tokensAs[PosLong] shouldBe posLong
    assertThrows[ReaderError](token(0L).tokensAs[PosLong])

    token(0L).tokensAs[NonNegLong] shouldBe nonNegLong
    assertThrows[ReaderError](token(-5L).tokensAs[NonNegLong])

    token(-5L).tokensAs[NegLong] shouldBe negLong
    assertThrows[ReaderError](token(5L).tokensAs[NegLong])

    // Double
    token(5.0).tokensAs[PosDouble] shouldBe posDouble
    assertThrows[ReaderError](token(0.0).tokensAs[PosDouble])

    token(0.0).tokensAs[NonNegDouble] shouldBe nonNegDouble
    assertThrows[ReaderError](token(-5.0).tokensAs[NonNegDouble])

    token(-5.0).tokensAs[NegDouble] shouldBe negDouble
    assertThrows[ReaderError](token(5.0).tokensAs[NegDouble])

    token(5.0).tokensAs[NonNaNDouble] shouldBe nonNaNDouble
    assertThrows[ReaderError](token(Double.NaN).tokensAs[NonNaNDouble])

    // Float
    token(5.0f).tokensAs[PosFloat] shouldBe posFloat
    assertThrows[ReaderError](token(0.0f).tokensAs[PosDouble])

    token(0.0f).tokensAs[NonNegFloat] shouldBe nonNegFloat
    assertThrows[ReaderError](token(-5.0f).tokensAs[NonNegFloat])

    token(-5.0f).tokensAs[NegFloat] shouldBe negFloat
    assertThrows[ReaderError](token(5.0f).tokensAs[NegDouble])

    token(5.0f).tokensAs[NonNaNFloat] shouldBe nonNaNFloat
    assertThrows[ReaderError](token(Float.NaN).tokensAs[NonNaNDouble])
  }

  it should "work with strings" in {
    token("192.168.0.1").tokensAs[String Refined IPv4] shouldBe ipV4

    assertThrows[ReaderError](token("aaa").tokensAs[String Refined IPv4])
  }

  it should "work with collections" in {
    arr("a", "b").tokensAs[List[String] Refined NonEmpty] shouldBe nel

    assertThrows[ReaderError](arr().tokensAs[List[String] Refined NonEmpty])
  }

  behavior of "RefinedKeyReader"

  it should "work with refined strings" in {
    type Limits = Map[String Refined IPv4, Int]

    val limits: Limits = Map(refineV[IPv4].unsafeFrom("192.168.0.1") -> 1, refineV[IPv4].unsafeFrom("192.168.1.1") -> 2)

    obj("192.168.0.1" -> 1, "192.168.1.1" -> 2).tokensAs[Limits] shouldBe limits

    assertThrows[ReaderError](obj("192.168.0.1" -> 1, "192.168.256.1" -> 2).tokensAs[Limits])
  }
}
