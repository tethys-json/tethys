package tethys.derivation

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.commons.TokenNode
import tethys.commons.TokenNode._
import tethys.derivation.ADTWithType._
import tethys.derivation.auto._
import tethys.derivation.semiauto._
import tethys.writers.instances.SimpleJsonObjectWriter
import tethys.writers.tokens.SimpleTokenWriter._
import tethys.{JsonObjectWriter, JsonWriter}

class AutoWriterDerivationTest extends AnyFlatSpec with Matchers {

  behavior of "auto derivation"
  it should "auto derive writer for simple classes tree" in {
    JsonTreeTestData(a = 5, b = false, c = C(D(1))).asTokenList shouldBe obj(
      "a" -> 5,
      "b" -> false,
      "c" -> obj(
        "d" -> obj(
          "a" -> 1
        )
      )
    )
  }

  it should "auto derive writers for a lot of embedded classes" in {
    Seq(SeqMaster1(Seq(SeqMaster2(Seq(SeqMaster3(Seq(SeqMaster4(Seq(1))))))))).asTokenList shouldBe arr(
      obj(
        "a" -> arr(obj(
          "a" -> arr(obj(
            "a" -> arr(obj(
              "a" -> arr(1)
            ))
          ))
        ))
      )
    )
  }

  it should "auto derive writer for recursive type" in {
    RecursiveType(1, Seq(RecursiveType(2))).asTokenList shouldBe obj(
      "a" -> 1,
      "children" -> arr(
        obj(
          "a" -> 2,
          "children" -> arr()
        )
      )
    )
  }

  it should "auto derive writer for A => B => A cycle" in {
    ComplexRecursionA(1, Some(ComplexRecursionB(2, ComplexRecursionA(3, None)))).asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> obj(
        "b" -> 2,
        "a" -> obj(
          "a" -> 3
        )
      )
    )
  }

  it should "auto derive writer for sealed cyclic trait with type parameter" in {
    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> obj(
        "a" -> 2
      )
    )
  }

  it should "auto derive writer that normally concatenates with other JsonObjectWriter's" in {
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]: JsonObjectWriter[ADTWithType[B]] = {
      val simpleJsonObjectWriter = SimpleJsonObjectWriter[ADTWithType[B]].addField("clazz") {
        case _: ADTWithTypeA[B] => "ADTWithTypeA"
        case _: ADTWithTypeB[B] => "ADTWithTypeB"
      }
      simpleJsonObjectWriter ++ jsonWriter[ADTWithType[B]]
    }

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asTokenList shouldBe obj(
      "clazz" -> "ADTWithTypeB",
      "a" -> 1,
      "b" -> obj(
        "clazz" -> "ADTWithTypeA",
        "a" -> 2
      )
    )
  }

  it should "not auto derive writer for sealed cyclic trait with type parameter if one of subclasses has additional type" in {
    "tethys.JsonWriter[ADTWithWrongType[Int]]" shouldNot compile
  }

  it should "auto derive writer for simple sealed trait with hierarchy" in {
    implicit val simpleClassWriter: JsonObjectWriter[SimpleClass] = JsonWriter.obj[SimpleClass].addField("b")(_.b)
    implicit val justObjectWriter: JsonObjectWriter[JustObject.type] = JsonWriter.obj.addField("type")(_ => "JustObject")

    implicit val sealedWriter: JsonWriter[SimpleSealedType] = jsonWriter[SimpleSealedType]

    def write(simpleSealedType: SimpleSealedType): List[TokenNode] = simpleSealedType.asTokenList

    write(CaseClass(1)) shouldBe obj("a" -> 1)
    write(new SimpleClass(2)) shouldBe obj("b" -> 2)
    write(JustObject) shouldBe obj("type" -> "JustObject")
    write(SubChild(3)) shouldBe obj("c" -> 3)
  }
}
