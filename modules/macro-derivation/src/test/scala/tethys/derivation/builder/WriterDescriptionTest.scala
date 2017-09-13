package tethys.derivation.builder

import org.scalatest.{FlatSpec, Matchers}
import tethys.core.writers.builder.{BuilderOperation, WriterBuilder, WriterDescription}
import tethys.derivation.builder.WriterDescriptionTest._
import tethys.derivation.semiauto._

class WriterDescriptionTest extends FlatSpec with Matchers {

  behavior of "Json.describe"
  it should "extract remove operations" in {
    describe {
      WriterBuilder[BuilderTestData]()
        .remove(_.a)
        .remove(_.inner)
    } shouldBe WriterDescription(Seq(
      BuilderOperation.Remove[BuilderTestData]("a"),
      BuilderOperation.Remove[BuilderTestData]("inner")
    ))
  }

  it should "not compile if we try to remove field from inner class" in {
    """
      |    extract {
      |      new JsonWriterBuilder[BuilderTestData]
      |        .remove(_.a)
      |        .remove(_.inner.a)
      |    }
    """.stripMargin shouldNot compile
  }

  it should "extract update operations" in {
    val description = describe {
      WriterBuilder[BuilderTestData]()
        .update(_.a)(_.toString)
    }

    val Seq(u: BuilderOperation.Update[BuilderTestData, Int, String]) = description.operations

    u.field shouldBe "a"
    u.fun(1) shouldBe "1"
  }

  it should "extract update partial operations" in {
    val description = describe {
      WriterBuilder[BuilderTestData]()
        .updatePartial(_.a) {
          case 1 => "uno!"
          case 2 => 4
          case v => v * 3
        }
    }

    val Seq(up: BuilderOperation.UpdatePartial[BuilderTestData, Int]) = description.operations

    up.field shouldBe "a"
    up.fun(1) shouldBe "uno!"
    up.fun(2) shouldBe 4
    up.fun(3) shouldBe 9
  }

  it should "extract complex case" in {

    val testData = BuilderTestData(1, "a", c = true, 4L, InnerCls(2))

    val WriterDescription(operations) = describe {
      WriterBuilder[BuilderTestData]()
        .remove(_.a)
        .update(_.c)(c => !c)
        .add("e")(_.a * 2)
    }

    operations.size shouldBe 3
    operations.head shouldBe BuilderOperation.Remove[BuilderTestData]("a")

    operations(1) shouldBe a[BuilderOperation.Update[_, _, _]]

    val u = operations(1).asInstanceOf[BuilderOperation.Update[BuilderTestData, Boolean, Boolean]]
    u.field shouldBe "c"
    u.fun(true) shouldBe false

    operations(2) shouldBe a[BuilderOperation.Add[_, _]]

    val add = operations(2).asInstanceOf[BuilderOperation.Add[BuilderTestData, Int]]
    add.field shouldBe "e"
    add.fun(testData) shouldBe 2
  }
}

object WriterDescriptionTest {

  case class BuilderTestData(a: Int, b: String, c: Boolean, d: Long, inner: InnerCls)

  case class InnerCls(a: Int)

}


