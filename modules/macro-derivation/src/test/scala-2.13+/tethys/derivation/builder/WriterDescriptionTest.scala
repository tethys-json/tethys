//package tethys.derivation.builder
//
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.flatspec.AnyFlatSpec
//import tethys.derivation.builder.WriterDescriptionTest._
//import tethys.derivation.builder.WriterDescription._
//import tethys.derivation.semiauto._
//
//class WriterDescriptionTest extends AnyFlatSpec with Matchers {
//
//  behavior of "Json.describe"
//  it should "extract derivation config" in {
//    describe {
//      WriterBuilder[BuilderTestData]
//        .fieldStyle(FieldStyle.lowerSnakecase)
//    } shouldBe WriterDescription(
//      WriterDerivationConfig.withFieldStyle(FieldStyle.lowerSnakecase),
//      Seq()
//    )
//  }
//
//  it should "extract remove operations" in {
//    describe {
//      WriterBuilder[BuilderTestData]
//        .remove(_.a)
//        .remove(_.inner)
//    } shouldBe WriterDescription(WriterDerivationConfig.empty, Seq(
//      BuilderOperation.Remove[BuilderTestData]("a"),
//      BuilderOperation.Remove[BuilderTestData]("inner")
//    ))
//  }
//
//  it should "not compile if we try to remove field from inner class" in {
//    """
//      |    extract {
//      |      new JsonWriterBuilder[
//      |        .remove(_.a)
//      |        .remove(_.inner.a)
//      |    }
//    """.stripMargin shouldNot compile
//  }
//
//  it should "extract update operations" in {
//    val description = describe {
//      WriterBuilder[BuilderTestData]
//        .update(_.a)(_.toString)
//    }
//
//    val Seq(u: BuilderOperation.Update[BuilderTestData, Int, String]) = description.operations
//
//    u.field shouldBe "a"
//    u.fun(1) shouldBe "1"
//  }
//
//  it should "extract update from root operations" in {
//    val description = describe {
//      WriterBuilder[BuilderTestData]
//        .update(_.a).fromRoot(_.a.toString)
//    }
//
//    val Seq(u: BuilderOperation.UpdateFromRoot[BuilderTestData, String]) = description.operations
//
//    u.field shouldBe "a"
//    u.fun(BuilderTestData(1, "s", c = true, 1L, InnerCls(2))) shouldBe "1"
//  }
//
//  it should "extract update partial operations" in {
//    val description = describe {
//      WriterBuilder[BuilderTestData]
//        .updatePartial(_.a) {
//          case 1 => "uno!"
//          case 2 => 4
//          case v => v * 3
//        }
//    }
//
//    val Seq(up: BuilderOperation.UpdatePartial[BuilderTestData, Int]) = description.operations
//
//    up.field shouldBe "a"
//    up.fun(1) shouldBe "uno!"
//    up.fun(2) shouldBe 4
//    up.fun(3) shouldBe 9
//  }
//
//  it should "extract update partial from root operations" in {
//    val description = describe {
//      WriterBuilder[BuilderTestData]
//        .updatePartial(_.a).fromRoot {
//          case d if d.a == 1 => "uno!"
//          case d if d.a == 2 => 4
//          case d => d.a * 3
//        }
//    }
//
//    val Seq(up: BuilderOperation.UpdatePartialFromRoot[BuilderTestData]) = description.operations
//
//    up.field shouldBe "a"
//
//    val data = BuilderTestData(1, "s", c = true, 1L, InnerCls(2))
//    up.fun(data) shouldBe "uno!"
//    up.fun(data.copy(a = 2)) shouldBe 4
//    up.fun(data.copy(a = 3)) shouldBe 9
//  }
//
//  it should "extract complex case" in {
//
//    val testData = BuilderTestData(1, "a", c = true, 4L, InnerCls(2))
//
//    val WriterDescription(_, operations) = describe {
//      WriterBuilder[BuilderTestData]
//        .remove(_.a)
//        .update(_.c)(c => !c)
//        .add("e")(_.a * 2)
//    }
//
//    operations.size shouldBe 3
//    operations.head shouldBe BuilderOperation.Remove[BuilderTestData]("a")
//
//    operations(1) shouldBe a[BuilderOperation.Update[_, _, _]]
//
//    val u = operations(1).asInstanceOf[BuilderOperation.Update[BuilderTestData, Boolean, Boolean]]
//    u.field shouldBe "c"
//    u.fun(true) shouldBe false
//
//    operations(2) shouldBe a[BuilderOperation.Add[_, _]]
//
//    val add = operations(2).asInstanceOf[BuilderOperation.Add[BuilderTestData, Int]]
//    add.field shouldBe "e"
//    add.fun(testData) shouldBe 2
//  }
//
//  it should "extract rename" in {
//    val WriterDescription(_, Seq(op: BuilderOperation.Update[BuilderTestData, Int, Int])) = describe {
//      WriterBuilder[BuilderTestData].rename(_.a)("aa")
//    }
//
//    op.field shouldBe "a"
//    op.name shouldBe Some("aa")
//    op.fun(1) shouldBe 1
//  }
//
//  it should "extract update with rename" in {
//    val fun: Int => Int = _ + 1
//
//    describe {
//      WriterBuilder[BuilderTestData].update(_.a).withRename("aa")(fun)
//    }.operations shouldBe Seq(
//      BuilderOperation.Update("a", Some("aa"), fun)
//    )
//  }
//
//  it should "extract update from root with rename" in {
//    val fun: PartialFunction[BuilderTestData, Int] = {
//      case d => d.a
//    }
//
//    describe {
//      WriterBuilder[BuilderTestData].update(_.a).withRename("aa").fromRoot(fun)
//    }.operations shouldBe Seq(
//      BuilderOperation.UpdateFromRoot("a", Some("aa"), fun)
//    )
//  }
//}
//
//object WriterDescriptionTest {
//
//  case class BuilderTestData(a: Int, b: String, c: Boolean, d: Long, inner: InnerCls)
//
//  case class InnerCls(a: Int)
//
//}
//
//
