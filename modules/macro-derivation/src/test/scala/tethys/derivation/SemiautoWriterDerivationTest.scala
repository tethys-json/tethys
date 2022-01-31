package tethys.derivation

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys.commons.TokenNode
import tethys.{JsonObjectWriter, JsonWriter}
import tethys.derivation.builder.{FieldStyle, WriterBuilder, WriterDerivationConfig}
import tethys.writers.tokens.SimpleTokenWriter._
import tethys.commons.TokenNode._
import tethys.derivation.ADTWithType.{ADTWithTypeA, ADTWithTypeB}
import tethys.derivation.semiauto._
import tethys.writers.instances.SimpleJsonObjectWriter

class SemiautoWriterDerivationTest extends AnyFlatSpec with Matchers {

  behavior of "semiauto derivation"
  it should "generate proper writer from WriterDescription" in {
    def freeVariable: String = "e"
    implicit val dWriter: JsonWriter[D] = jsonWriter[D](WriterDerivationConfig.withFieldStyle(FieldStyle.Uppercase))

    implicit val testWriter: JsonWriter[JsonTreeTestData] = jsonWriter {
      WriterBuilder[JsonTreeTestData]
        .remove(_.b)
        .update(_.a).fromRoot(d => d.a.toDouble + d.c.d.a)
        .update(_.c)(_.d)
        .add("d")(_.a * 2)
        .add(freeVariable)(_.b)
    }
    JsonTreeTestData(5, b = false, C(D(1))).asTokenList shouldBe obj(
      "a" -> 6.0,
      "c" -> obj(
        "A" -> 1
      ),
      "d" -> 10,
      "e" -> false
    )
  }

  it should "derive writer for update partial" in {
    implicit val partialWriter: JsonWriter[D] = jsonWriter {
      describe {
        WriterBuilder[D]
          .updatePartial(_.a) {
            case 1 => "uno!"
            case 2 => 1
            case v if v > 0 => v * 2
            case _ => throw new IllegalArgumentException("Wrong value!")
          }
      }
    }

    D(1).asTokenList shouldBe obj("a" -> "uno!")
    D(2).asTokenList shouldBe obj("a" -> 1)
    D(3).asTokenList shouldBe obj("a" -> 6)

    intercept[IllegalArgumentException] {
      D(0).asTokenList
    }.getMessage shouldBe "Wrong value!"
  }

  it should "derive writer for update partial from root" in {
    implicit val partialWriter: JsonWriter[D] = jsonWriter {
      describe {
        WriterBuilder[D]
          .updatePartial(_.a).fromRoot {
            case d if d.a == 1 => "uno!"
            case d if d.a == 2 => 1
            case d if d.a > 0 => d.a * 2
            case _ => throw new IllegalArgumentException("Wrong value!")
          }
      }
    }

    D(1).asTokenList shouldBe obj("a" -> "uno!")
    D(2).asTokenList shouldBe obj("a" -> 1)
    D(3).asTokenList shouldBe obj("a" -> 6)

    intercept[IllegalArgumentException] {
      D(0).asTokenList
    }.getMessage shouldBe "Wrong value!"
  }

  it should "not compile if called on trait for simple writer" in {
    "jsonWriter[JsonComplexTestData]" shouldNot compile
  }

  it should "derive writer for recursive type" in {
    implicit lazy val testWriter: JsonWriter[RecursiveType] = jsonWriter[RecursiveType]

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

  it should "derive writer for A => B => A cycle" in {
    implicit lazy val testWriter1: JsonWriter[ComplexRecursionA] = jsonWriter[ComplexRecursionA]
    implicit lazy val testWriter2: JsonWriter[ComplexRecursionB] = jsonWriter[ComplexRecursionB]

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

  it should "derive writer for sealed cyclic trait with type parameter" in {
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]: JsonObjectWriter[ADTWithType[B]] = jsonWriter[ADTWithType[B]]
    implicit def recursionTraitWithTypeAWriter[B: JsonWriter]: JsonObjectWriter[ADTWithTypeA[B]] = jsonWriter[ADTWithTypeA[B]]
    implicit def recursionTraitWithTypeBWriter[B: JsonWriter]: JsonObjectWriter[ADTWithTypeB[B]] = jsonWriter[ADTWithTypeB[B]]

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> obj(
        "a" -> 2
      )
    )
  }

  it should "derive writer that normally concatenates with other JsonWriter's" in {
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]: JsonWriter[ADTWithType[B]] = {
      val simpleJsonWriter = SimpleJsonObjectWriter[ADTWithType[B]].addField("clazz") {
        case _: ADTWithTypeA[B] => "ADTWithTypeA"
        case _: ADTWithTypeB[B] => "ADTWithTypeB"
      }
      simpleJsonWriter ++ jsonWriter[ADTWithType[B]]
    }
    implicit def recursionTraitWithTypeAWriter[B: JsonWriter]: JsonObjectWriter[ADTWithTypeA[B]] = jsonWriter[ADTWithTypeA[B]]
    implicit def recursionTraitWithTypeBWriter[B: JsonWriter]: JsonObjectWriter[ADTWithTypeB[B]] = jsonWriter[ADTWithTypeB[B]]

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asTokenList shouldBe obj(
      "clazz" -> "ADTWithTypeB",
      "a" -> 1,
      "b" -> obj(
        "clazz" -> "ADTWithTypeA",
        "a" -> 2
      )
    )
  }

  it should "not derive writer for sealed cyclic trait with type parameter if one of subclasses has additional type" in {
    """
      implicit def recursionTraitWithTypeWriter[A: JsonWriter]: JsonWriter[ADTWithWrongType[A]] = jsonWriter[ADTWithWrongType[A]]
      implicit def recursionTraitWithTypeAWriter[A: JsonWriter]: JsonWriter[ADTWithWrongType.ADTWithWrongTypeA[A]] = jsonWriter[ADTWithWrongType.ADTWithWrongTypeA[A]]
      implicit def recursionTraitWithTypeAWriter[A: JsonWriter, B: JsonWriter]: JsonWriter[ADTWithWrongType.ADTWithWrongTypeB[A]] = jsonWriter[ADTWithWrongType.ADTWithWrongTypeB[A]]
    """ shouldNot compile
  }

  it should "derive writer for simple sealed trait with hierarchy" in {
    implicit val caseClassWriter: JsonObjectWriter[CaseClass] = jsonWriter[CaseClass]
    implicit val simpleClassWriter: JsonObjectWriter[SimpleClass] = JsonWriter.obj[SimpleClass].addField("b")(_.b)
    implicit val justObjectWriter: JsonObjectWriter[JustObject.type] = JsonWriter.obj.addField("type")(_ => "JustObject")
    implicit val subChildWriter: JsonObjectWriter[SubChild] = jsonWriter[SubChild]

    implicit val sealedWriter: JsonWriter[SimpleSealedType] = jsonWriter[SimpleSealedType]

    def write(simpleSealedType: SimpleSealedType): List[TokenNode] = simpleSealedType.asTokenList

    write(CaseClass(1)) shouldBe obj("a" -> 1)
    write(new SimpleClass(2)) shouldBe obj("b" -> 2)
    write(JustObject) shouldBe obj("type" -> "JustObject")
    write(SubChild(3)) shouldBe obj("c" -> 3)
  }

  it should "derive writer for simple sealed trait with hierarchy with discriminator" in {
    implicit val caseClassWriter: JsonObjectWriter[CaseClass] = jsonWriter[CaseClass]
    implicit val simpleClassWriter: JsonObjectWriter[SimpleClass] = JsonWriter.obj[SimpleClass].addField("b")(_.b)
    implicit val justObjectWriter: JsonObjectWriter[JustObject.type] = JsonWriter.obj
    implicit val subChildWriter: JsonObjectWriter[SubChild] = jsonWriter[SubChild]

    implicit val sealedWriter: JsonWriter[SimpleSealedType] = jsonWriter[SimpleSealedType](
      WriterDerivationConfig.empty.withDiscriminator("__type")
    )

    def write(simpleSealedType: SimpleSealedType): List[TokenNode] = simpleSealedType.asTokenList

    write(CaseClass(1)) shouldBe obj("a" -> 1, "__type" -> "CaseClass")
    write(new SimpleClass(2)) shouldBe obj("b" -> 2, "__type" -> "SimpleClass")
    write(JustObject) shouldBe obj("__type" -> "JustObject")
    write(SubChild(3)) shouldBe obj("c" -> 3, "__type" -> "SubChild")
  }
}
