package tethys.cats

import cats.collections.{
  AvlSet,
  BitSet,
  Dequeue,
  HashMap,
  HashSet,
  Heap,
  PairingHeap,
  TreeList
}
import cats.data._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode._
import instances._
import tethys.readers.ReaderError
import tethys.writers.tokens.SimpleTokenWriter._

class CatsSupportTests extends AnyFlatSpec with Matchers {
  val nev: NonEmptyVector[String] = NonEmptyVector.of("a", "b")
  val nel: NonEmptyList[Int] = NonEmptyList.of(1, 2)
  val nes: NonEmptySet[Int] = NonEmptySet.of(1, 2, 3, 4)
  val chain: Chain[Int] = Chain.fromIterableOnce(Seq(1, 2, 3))
  val nec: NonEmptyChain[String] = NonEmptyChain.of("a", "b", "c")

  val bitSet: BitSet = BitSet.apply(5, 1, 5, 4, 2, 3)
  val dequeue: Dequeue[String] = Dequeue("a", "b", "c")
  val hashMap: HashMap[String, Int] = HashMap("a" -> 1, "b" -> 2)
  val hasSet: HashSet[Int] = HashSet(1, 2, 3)
  val heap: Heap[Int] = Heap.fromIterable(Seq(1, 2, 3, 4))
  val pairingHeap: PairingHeap[Int] = PairingHeap.fromIterable(Seq(1, 2, 3))
  val treeList: TreeList[Int] = TreeList.fromList(List(1, 2, 3, 4))
  val avlSet: AvlSet[Int] = AvlSet.fromList(List(1, 2, 3))

  behavior of "CatsWriters"
  it should "write non-empty" in {
    nev.asTokenList shouldBe arr("a", "b")
    nel.asTokenList shouldBe arr(1, 2)
    nes.asTokenList shouldBe arr(1, 2, 3, 4)
    chain.asTokenList shouldBe arr(1, 2, 3)
    nec.asTokenList shouldBe arr("a", "b", "c")
  }

  it should "write cats-collection" in {
    bitSet.asTokenList shouldBe arr(1, 2, 3, 4, 5)
    dequeue.asTokenList shouldBe arr("a", "b", "c")
    hashMap.asTokenList shouldBe obj("a" -> 1, "b" -> 2)
    hasSet.asTokenList shouldBe arr(1, 2, 3)
    heap.asTokenList shouldBe arr(1, 2, 3, 4)
    pairingHeap.asTokenList shouldBe arr(1, 2, 3)
    treeList.asTokenList shouldBe arr(1, 2, 3, 4)
    avlSet.asTokenList shouldBe arr(1, 2, 3)
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

  it should "read cats-collection" in {
    bitSet shouldBe arr(1, 2, 3, 4, 5).tokensAs[BitSet]
    dequeue shouldBe arr("a", "b", "c").tokensAs[Dequeue[String]]
    hashMap shouldBe obj("a" -> 1, "b" -> 2).tokensAs[HashMap[String, Int]]
    hasSet shouldBe arr(1, 2, 3).tokensAs[HashSet[Int]]
    heap shouldBe arr(1, 2, 3, 4).tokensAs[Heap[Int]]
    pairingHeap shouldBe arr(1, 2, 3).tokensAs[PairingHeap[Int]]
    treeList shouldBe arr(1, 2, 3, 4).tokensAs[TreeList[Int]]
    avlSet shouldBe arr(1, 2, 3).tokensAs[AvlSet[Int]]
  }
}
