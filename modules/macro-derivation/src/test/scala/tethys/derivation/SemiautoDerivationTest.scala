package tethys.derivation

import org.scalatest.{FlatSpec, Matchers}
import tethys.core.writers.JsonWriter
import tethys.core.writers.builder.WriterBuilder
import tethys.derivation.ADTWithType.{ADTWithTypeA, ADTWithTypeB}
import tethys.derivation.semiauto._
import tethys.core.writers.syntax._
import ADTWithType.{ADTWithTypeA, ADTWithTypeB}

/**
  * Created by eld0727 on 23.04.17.
  */
class SemiautoDerivationTest extends FlatSpec with Matchers {

  it should "generate proper writer from WriterDescription" in {
    implicit val dWriter: JsonWriter[D] = jsonWriter[D]

    implicit val testWriter: JsonWriter[JsonWriterTestData] = jsonWriter {
      describe {
        WriterBuilder[JsonWriterTestData]()
          .remove(_.b)
          .update(_.a)(_ + 1.0)
          .update(_.c)(_.d)
          .add("d")(_.a * 2)
          .add("e")(_.b)
      }
    }
    writeString(JsonWriterTestData(5, b = false, C(D(1)))).shouldBe(
      """{"a":6.0,"c":{"a":1},"d":10,"e":false}"""
    )
  }

  it should "derive writer for update partial" in {
    implicit val partialWriter: JsonWriter[D] = jsonWriter {
      describe {
        WriterBuilder[D]()
          .updatePartial(_.a) {
            case 1 => "uno!"
            case 2 => 1
            case v if v > 0 => v * 2
            case _ => throw new IllegalArgumentException("Wrong value!")
          }
      }
    }

    writeString(D(1)) shouldBe """{"a":"uno!"}"""
    writeString(D(2)) shouldBe """{"a":1}"""
    writeString(D(3)) shouldBe """{"a":6}"""
    intercept[IllegalArgumentException] {
      writeString(D(0))
    }.getMessage shouldBe "Wrong value!"
  }

  it should "not compile if called on trait for simple writer" in {
    "jsonWriter[JsonComplexTestData]" shouldNot compile
  }

  it should "derive writer for recursive type" in {
    implicit lazy val testWriter: JsonWriter[RecursiveType] = jsonWriter[RecursiveType]

    writeString(RecursiveType(1, Seq(RecursiveType(2)))).shouldBe(
      """{"a":1,"children":[{"a":2,"children":[]}]}"""
    )
  }

  it should "derive writer for A => B => A cycle" in {
    implicit lazy val testWriter1: JsonWriter[ComplexRecursionA] = jsonWriter[ComplexRecursionA]
    implicit lazy val testWriter2: JsonWriter[ComplexRecursionB] = jsonWriter[ComplexRecursionB]

    writeString(ComplexRecursionA(1, Some(ComplexRecursionB(2, ComplexRecursionA(3, None))))).shouldBe(
      """{"a":1,"b":{"b":2,"a":{"a":3}}}"""
    )
  }

  it should "derive writer for sealed cyclic trait with type parameter" in {
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]: JsonWriter[ADTWithType[B]] = jsonWriter[ADTWithType[B]]
    implicit def recursionTraitWithTypeAWriter[B: JsonWriter]: JsonWriter[ADTWithTypeA[B]] = jsonWriter[ADTWithTypeA[B]]
    implicit def recursionTraitWithTypeBWriter[B: JsonWriter]: JsonWriter[ADTWithTypeB[B]] = jsonWriter[ADTWithTypeB[B]]

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[Int]).asJson.shouldBe(
      """{"a":1,"b":{"a":2}}"""
    )
  }

  it should "not derive writer for sealed cyclic trait with type parameter if one of subclasses has additional type" in {
    """
      implicit def recursionTraitWithTypeWriter[A: JsonWriter]: JsonWriter[ADTWithWrongType[A]] = jsonWriter[ADTWithWrongType[A]]
      implicit def recursionTraitWithTypeAWriter[A: JsonWriter]: JsonWriter[ADTWithWrongType.ADTWithWrongTypeA[A]] = jsonWriter[ADTWithWrongType.ADTWithWrongTypeA[A]]
      implicit def recursionTraitWithTypeAWriter[A: JsonWriter, B: JsonWriter]: JsonWriter[ADTWithWrongType.ADTWithWrongTypeB[A]] = jsonWriter[ADTWithWrongType.ADTWithWrongTypeB[A]]
    """ shouldNot compile
  }
}
