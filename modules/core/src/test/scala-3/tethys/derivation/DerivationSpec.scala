package tethys.derivation

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tethys.commons.TokenNode.{arr, obj}
import tethys.commons.{Token, TokenNode}
import tethys.readers.tokens.QueueIterator
import tethys.writers.tokens.SimpleTokenWriter.SimpleTokenWriterOps
import tethys.*
import tethys.derivation.ADTWithType.{ADTWithTypeA, ADTWithTypeB}
import tethys.readers.ReaderError
import tethys.writers.instances.SimpleJsonObjectWriter

class DerivationSpec extends AnyFlatSpec with Matchers {

  def read[A: JsonReader](nodes: List[TokenNode]): A = {
    val it = QueueIterator(nodes)
    val res = it.readJson[A].fold(throw _, identity)
    it.currentToken() shouldBe Token.Empty
    res
  }

  it should "derive sum type for opaque types" in {
    import tethys.*

    object Id:
      opaque type Id = Long
      given JsonReader[Id] = JsonReader.longReader

      object Id:
        def apply(i: Long): Id = i

    enum Foo(@selector val typ: String) derives JsonReader:
      case Bar(id: Id.Id) extends Foo("foo")

    read[Foo](obj("typ" -> "foo", "id" -> 1)) shouldBe Foo.Bar(Id.Id(1))
  }

  it should "build message correctly" in {
    case class Foo(bar: Int, baz: String, faz: Boolean) derives JsonReader

    util.Try(read[Foo](obj("bar" -> 1))) should matchPattern {
      case util.Failure(ex)
          if ex.getMessage == """Illegal json at '[ROOT]': Can not extract fields from json: 'baz', 'faz'""" =>
    }
  }

  it should "compile and correctly write and read product" in {
    case class Person(
        id: Int,
        name: String,
        phone: Option[String],
        default: String = ""
    ) derives JsonObjectWriter,
          JsonReader

    case class Wrapper(person: Person) derives JsonObjectWriter, JsonReader

    Person(2, "Peter", None).asTokenList shouldBe obj(
      "id" -> 2,
      "name" -> "Peter",
      "default" -> ""
    )

    Wrapper(Person(3, "Parker", None, "abc")).asTokenList shouldBe obj(
      "person" -> obj("id" -> 3, "name" -> "Parker", "default" -> "abc")
    )

    read[Person](obj("id" -> 1, "name" -> "abc")) shouldBe Person(
      1,
      "abc",
      None
    )

    read[Person](
      obj(
        "abd" -> 3,
        "name" -> "abc",
        "id" -> 1,
        "default" -> "abc"
      )
    ) shouldBe Person(1, "abc", None, "abc")

    read[Wrapper](
      obj(
        "abc" -> 5,
        "person" -> obj("id" -> 3, "name" -> "Parker", "phone" -> "123")
      )
    ) shouldBe Wrapper(Person(3, "Parker", Some("123")))
  }

  it should "compile and correctly write sum" in {
    sealed trait A derives JsonObjectWriter

    case class B(b: Int, i: String) extends A derives JsonObjectWriter

    case class C(c: String) extends A derives JsonObjectWriter

    (B(2, "abc"): A).asTokenList shouldBe obj(
      "b" -> 2,
      "i" -> "abc"
    )
  }

  it should "compile and correctly read/write enum with StringEnumWriter" in {
    enum A derives StringEnumJsonWriter, StringEnumJsonReader:
      case B, C

    A.B.asTokenList shouldBe TokenNode.value("B")
    A.C.asTokenList shouldBe TokenNode.value("C")

    read[A](
      TokenNode.value("B")
    ) shouldBe A.B

    read[A](
      TokenNode.value("C")
    ) shouldBe A.C
  }

  it should "compile and correctly read/write enum with custom StringEnumWriter" in {
    enum A(val value: String):
      case B extends A("b")
      case C extends A("c")

    object A:
      given StringEnumJsonWriter[A] = StringEnumJsonWriter.derived(_.value)
      given StringEnumJsonReader[A] = StringEnumJsonReader.derived(_.value)

    A.B.asTokenList shouldBe TokenNode.value("b")
    A.C.asTokenList shouldBe TokenNode.value("c")

    read[A](
      TokenNode.value("b")
    ) shouldBe A.B

    read[A](
      TokenNode.value("c")
    ) shouldBe A.C
  }

  it should "compile and correctly read/write enum with OrdinalEnumWriter" in {
    enum A derives OrdinalEnumJsonWriter, OrdinalEnumJsonReader:
      case B, C

    A.B.asTokenList shouldBe TokenNode.value(0)
    A.C.asTokenList shouldBe TokenNode.value(1)

    read[A](
      TokenNode.value(0)
    ) shouldBe A.B

    read[A](
      TokenNode.value(1)
    ) shouldBe A.C
  }

  it should "compile and correctly write enum obj with discriminator" in {
    enum A:
      case B, C

    {
      given JsonWriter[A] = StringEnumJsonWriter.withLabel("__type")
      A.B.asTokenList shouldBe obj("__type" -> "B")
      A.C.asTokenList shouldBe obj("__type" -> "C")
    }

    {
      given JsonWriter[A] = OrdinalEnumJsonWriter.withLabel("__type")

      A.B.asTokenList shouldBe obj("__type" -> 0)
      A.C.asTokenList shouldBe obj("__type" -> 1)
    }
  }

  it should "correctly read case classes with default parameters" in {
    object Mod {
      case class WithOpt(x: Int, y: Option[String] = Some("default"))
          derives JsonReader
    }

    read[Mod.WithOpt](obj("x" -> 5)) shouldBe Mod.WithOpt(5)
  }

  it should "correctly read case classes with default parameters and type arguments" in {
    case class WithArg[A](x: Int, y: Option[A] = None) derives JsonReader

    read[WithArg[Int]](obj("x" -> 5)) shouldBe WithArg[Int](5)
    read[WithArg[String]](obj("x" -> 5, "y" -> "lool")) shouldBe WithArg[
      String
    ](5, Some("lool"))
  }

  it should "write/read sum types with provided json discriminator" in {
    enum Disc derives StringEnumJsonWriter, StringEnumJsonReader:
      case A, B

    sealed trait Choose(@selector val discriminator: Disc)
        derives JsonObjectWriter,
          JsonReader

    object Choose:
      case class AA() extends Choose(Disc.A)
      case class BB() extends Choose(Disc.B)

    (Choose.AA(): Choose).asTokenList shouldBe obj("discriminator" -> "A")
    (Choose.BB(): Choose).asTokenList shouldBe obj("discriminator" -> "B")

    read[Choose](obj("discriminator" -> "A")) shouldBe Choose.AA()
    read[Choose](obj("discriminator" -> "B")) shouldBe Choose.BB()
  }

  it should "write/read sum types with provided json discriminator of simple type" in {
    enum Choose(@selector val discriminator: Int)
        derives JsonObjectWriter,
          JsonReader:
      case AA() extends Choose(0)
      case BB() extends Choose(1)

    (Choose.AA(): Choose).asTokenList shouldBe obj("discriminator" -> 0)
    (Choose.BB(): Choose).asTokenList shouldBe obj("discriminator" -> 1)

    read[Choose](obj("discriminator" -> 0)) shouldBe Choose.AA()
    read[Choose](obj("discriminator" -> 1)) shouldBe Choose.BB()
  }

  it should "not compile derivation when discriminator override found" in {

    """
      |
      |    sealed trait Foo(@selector val x: Int) derives JsonReader, JsonObjectWriter
      |
      |    object Foo:
      |      given JsonDiscriminator[Foo, Int] = JsonDiscriminator.by(_.x)
      |
      |      case class Bar(override val x: Int) extends Foo(x)
      |
      |      case class Baz() extends Foo(0)
      |
      |""" shouldNot compile

  }

  it should "derive readers for simple case class hierarchy" in {
    implicit val dReader: JsonReader[D] = JsonReader.derived[D]
    implicit val cReader: JsonReader[C] = JsonReader.derived[C]
    implicit val jsonTreeTestDataReader: JsonReader[JsonTreeTestData] =
      JsonReader.derived[JsonTreeTestData]

    read[JsonTreeTestData](
      obj(
        "a" -> 1,
        "b" -> true,
        "c" -> obj(
          "d" -> obj(
            "a" -> 2
          )
        )
      )
    ) shouldBe JsonTreeTestData(
      a = 1,
      b = true,
      c = C(D(2))
    )
  }

  it should "derive reader for recursive type" in {
    given JsonReader[RecursiveType] = JsonReader.derived[RecursiveType]

    read[RecursiveType](
      obj(
        "a" -> 1,
        "children" -> arr(
          obj(
            "a" -> 2,
            "children" -> arr()
          ),
          obj(
            "a" -> 3,
            "children" -> arr()
          )
        )
      )
    ) shouldBe RecursiveType(1, Seq(RecursiveType(2), RecursiveType(3)))

  }

  it should "derive reader for A => B => A cycle" in {
    implicit lazy val testReader1: JsonReader[ComplexRecursionA] =
      JsonReader.derived[ComplexRecursionA]
    implicit lazy val testReader2: JsonReader[ComplexRecursionB] =
      JsonReader.derived[ComplexRecursionB]

    read[ComplexRecursionA](
      obj(
        "a" -> 1,
        "b" -> obj(
          "b" -> 2,
          "a" -> obj(
            "a" -> 3
          )
        )
      )
    ) shouldBe ComplexRecursionA(
      1,
      Some(ComplexRecursionB(2, ComplexRecursionA(3, None)))
    )
  }

  it should "derive reader for extract as description" in {
    given JsonReader[SimpleType] = JsonReader.derived[SimpleType] {
      ReaderBuilder[SimpleType]
        .extract(_.i)
        .as[Option[Int]](_.getOrElse(2))
    }

    read[SimpleType](
      obj(
        "i" -> 1,
        "s" -> "str",
        "d" -> 1.0
      )
    ) shouldBe SimpleType(1, "str", 1.0)

    read[SimpleType](
      obj(
        "s" -> "str",
        "d" -> 1.0
      )
    ) shouldBe SimpleType(2, "str", 1.0)
  }

  it should "work for deprecated ReaderBuilder" in {
    import tethys.derivation.builder.ReaderBuilder
    given JsonReader[SimpleType] = JsonReader.derived[SimpleType] {
      ReaderBuilder[SimpleType]
        .extract(_.i)
        .as[Option[Int]](_.getOrElse(2))
    }

    read[SimpleType](
      obj(
        "i" -> 1,
        "s" -> "str",
        "d" -> 1.0
      )
    ) shouldBe SimpleType(1, "str", 1.0)

    read[SimpleType](
      obj(
        "s" -> "str",
        "d" -> 1.0
      )
    ) shouldBe SimpleType(2, "str", 1.0)
  }

  it should "derive reader for extract from description" in {
    given JsonReader[SimpleType] = JsonReader.derived {
      ReaderBuilder[SimpleType]
        .extract(_.i)
        .from(_.s)
        .and(_.d)
        .apply((_, _) => 2)
    }

    read[SimpleType](
      obj(
        "i" -> 1,
        "s" -> "str",
        "d" -> 1.0
      )
    ) shouldBe SimpleType(2, "str", 1.0)

    read[SimpleType](
      obj(
        "s" -> "str",
        "d" -> 1.0
      )
    ) shouldBe SimpleType(2, "str", 1.0)
  }

  it should "derive reader for extract from description with synthetic field" in {
    given JsonReader[SimpleType] = JsonReader.derived[SimpleType] {
      ReaderBuilder[SimpleType]
        .extract(_.i)
        .from(_.d)
        .and[Double]("e")((d, e) => (d + e).toInt)
    }

    read[SimpleType](
      obj(
        "i" -> 1,
        "s" -> "str",
        "d" -> 1.0,
        "e" -> 2.0
      )
    ) shouldBe SimpleType(3, "str", 1.0)

    read[SimpleType](
      obj(
        "s" -> "str",
        "d" -> 1.0,
        "e" -> 3.0
      )
    ) shouldBe SimpleType(4, "str", 1.0)
  }

  it should "extract and build product" in {
    case class Person(name: String, age: Int)
    case class Wrapper(person: Person) derives JsonReader

    inline given ReaderBuilder[Wrapper] = ReaderBuilder[Wrapper]
      .extract(_.person)
      .from[String]("name")
      .and[Int]("age")
      .product

    read[Wrapper](
      obj(
        "name" -> "str",
        "age" -> 2
      )
    ) shouldBe Wrapper(Person("str", 2))
  }

  it should "derive reader for extract reader from description" in {
    given JsonReader[SimpleTypeWithAny] =
      JsonReader.derived[SimpleTypeWithAny] {
        ReaderBuilder[SimpleTypeWithAny]
          .extractReader(_.any)
          .from(_.d) {
            case 1.0 => JsonReader[String]
            case 2.0 => JsonReader[Int]
          }
      }

    read[SimpleTypeWithAny](
      obj(
        "i" -> 1,
        "s" -> "str",
        "d" -> 1.0,
        "any" -> "anyStr"
      )
    ) shouldBe SimpleTypeWithAny(1, "str", 1.0, "anyStr")

    read[SimpleTypeWithAny](
      obj(
        "i" -> 1,
        "s" -> "str",
        "d" -> 2.0,
        "any" -> 2
      )
    ) shouldBe SimpleTypeWithAny(1, "str", 2.0, 2)
  }

  it should "derive reader for complex extraction case" in {
    given JsonReader[SimpleTypeWithAny] =
      JsonReader.derived[SimpleTypeWithAny] {
        ReaderBuilder[SimpleTypeWithAny]
          .extractReader(_.any)
          .from(_.i) {
            case 1 => JsonReader[String]
            case 2 => JsonReader[Int]
            case _ => JsonReader[Option[Boolean]]
          }
          .extract(_.i)
          .from(_.d)
          .and[Int]("e")((d, e) => d.toInt + e)
          .extract(_.d)
          .as[Option[Double]](_.getOrElse(1.0))
      }

    read[SimpleTypeWithAny](
      obj(
        "s" -> "str",
        "d" -> 1.0,
        "e" -> 0,
        "any" -> "anyStr"
      )
    ) shouldBe SimpleTypeWithAny(1, "str", 1.0, "anyStr")

    read[SimpleTypeWithAny](
      obj(
        "s" -> "str",
        "d" -> 1.0,
        "e" -> 1,
        "any" -> 3
      )
    ) shouldBe SimpleTypeWithAny(2, "str", 1.0, 3)

    read[SimpleTypeWithAny](
      obj(
        "s" -> "str",
        "d" -> 1.0,
        "e" -> 2,
        "any" -> true
      )
    ) shouldBe SimpleTypeWithAny(3, "str", 1.0, Some(true))

    read[SimpleTypeWithAny](
      obj(
        "s" -> "str",
        "d" -> 1.0,
        "e" -> 2
      )
    ) shouldBe SimpleTypeWithAny(3, "str", 1.0, None)
  }

  it should "derive reader for fieldStyle from description" in {
    given JsonReader[CamelCaseNames] = JsonReader.derived[CamelCaseNames] {
      ReaderBuilder[CamelCaseNames]
        .fieldStyle(FieldStyle.LowerSnakeCase)
    }

    read[CamelCaseNames](
      obj(
        "some_param" -> 1,
        "id_param" -> 2,
        "simple" -> 3
      )
    ) shouldBe CamelCaseNames(
      someParam = 1,
      IDParam = 2,
      simple = 3
    )
  }

  it should "derive reader for fieldStyle from function in description" in {
    given JsonReader[CamelCaseNames] = JsonReader.derived[CamelCaseNames] {
      ReaderBuilder[CamelCaseNames]
        .fieldStyle(FieldStyle.Capitalize)
    }

    read[CamelCaseNames](
      obj(
        "SomeParam" -> 1,
        "IDParam" -> 2,
        "Simple" -> 3
      )
    ) shouldBe CamelCaseNames(
      someParam = 1,
      IDParam = 2,
      simple = 3
    )
  }

  it should "derive reader for reader config" in {
    given JsonReader[CamelCaseNames] = JsonReader.derived[CamelCaseNames](
      ReaderBuilder[CamelCaseNames]
        .fieldStyle(FieldStyle.LowerSnakeCase)
        .strict
    )

    read[CamelCaseNames](
      obj(
        "some_param" -> 1,
        "id_param" -> 2,
        "simple" -> 3
      )
    ) shouldBe CamelCaseNames(
      someParam = 1,
      IDParam = 2,
      simple = 3
    )

    (the[ReaderError] thrownBy {
      read[CamelCaseNames](
        obj(
          "some_param" -> 1,
          "not_id_param" -> 2,
          "simple" -> 3
        )
      )
    }).getMessage shouldBe "Illegal json at '[ROOT]': unexpected field 'not_id_param', expected one of 'some_param', 'id_param', 'simple'"
  }

  it should "derive reader for reader config from builder" in {
    implicit val reader: JsonReader[CamelCaseNames] =
      JsonReader.derived[CamelCaseNames](
        ReaderBuilder[CamelCaseNames].strict
          .fieldStyle(FieldStyle.LowerSnakeCase)
      )

    read[CamelCaseNames](
      obj(
        "some_param" -> 1,
        "id_param" -> 2,
        "simple" -> 3
      )
    ) shouldBe CamelCaseNames(
      someParam = 1,
      IDParam = 2,
      simple = 3
    )

    (the[ReaderError] thrownBy {
      read[CamelCaseNames](
        obj(
          "some_param" -> 1,
          "not_id_param" -> 2,
          "simple" -> 3
        )
      )
    }).getMessage shouldBe "Illegal json at '[ROOT]': unexpected field 'not_id_param', expected one of 'some_param', 'id_param', 'simple'"
  }

  it should "generate proper writer from WriterDescription" in {
    def freeVariable: String = "e"

    implicit val dWriter: JsonWriter[D] = JsonWriter.derived[D](
      WriterBuilder[D].fieldStyle(FieldStyle.UpperCase)
    )

    implicit val testWriter: JsonWriter[JsonTreeTestData] = JsonWriter.derived {
      WriterBuilder[JsonTreeTestData]
        .remove(_.b)
        .update(_.a)
        .fromRoot(d => d.a.toDouble + d.c.d.a)
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

  it should "work for deprecated WriterBuilder" in {
    def freeVariable: String = "e"
    import tethys.derivation.builder.WriterBuilder

    implicit val dWriter: JsonWriter[D] = JsonWriter.derived[D](
      WriterBuilder[D].fieldStyle(FieldStyle.UpperCase)
    )

    implicit val testWriter: JsonWriter[JsonTreeTestData] = JsonWriter.derived {
      WriterBuilder[JsonTreeTestData]
        .remove(_.b)
        .update(_.a)
        .fromRoot(d => d.a.toDouble + d.c.d.a)
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
    inline given WriterBuilder[D] = WriterBuilder[D]
      .update(_.a) {
        case 1          => "uno!"
        case 2          => 1
        case v if v > 0 => v * 2
        case _          => throw new IllegalArgumentException("Wrong value!")
      }

    given JsonWriter[D] = JsonWriter.derived

    D(1).asTokenList shouldBe obj("a" -> "uno!")
    D(2).asTokenList shouldBe obj("a" -> 1)
    D(3).asTokenList shouldBe obj("a" -> 6)

    intercept[IllegalArgumentException] {
      D(0).asTokenList
    }.getMessage shouldBe "Wrong value!"
  }

  it should "derive writer for update partial from root" in {
    implicit val partialWriter: JsonWriter[D] = JsonWriter.derived[D] {
      WriterBuilder[D]
        .update(_.a)
        .fromRoot {
          case d if d.a == 1 => "uno!"
          case d if d.a == 2 => 1
          case d if d.a > 0  => d.a * 2
          case _ => throw new IllegalArgumentException("Wrong value!")
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
    "JsonWriter.derived[JsonComplexTestData]" shouldNot compile
  }

  it should "derive writer for recursive type" in {
    implicit lazy val testWriter: JsonWriter[RecursiveType] =
      JsonWriter.derived[RecursiveType]

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
    implicit lazy val testWriter1: JsonWriter[ComplexRecursionA] =
      JsonWriter.derived[ComplexRecursionA]
    implicit lazy val testWriter2: JsonWriter[ComplexRecursionB] =
      JsonWriter.derived[ComplexRecursionB]

    ComplexRecursionA(
      1,
      Some(ComplexRecursionB(2, ComplexRecursionA(3, None)))
    ).asTokenList shouldBe obj(
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
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]
        : JsonObjectWriter[ADTWithType[B]] = JsonWriter.derived[ADTWithType[B]]

    implicit def recursionTraitWithTypeAWriter[B: JsonWriter]
        : JsonObjectWriter[ADTWithTypeA[B]] =
      JsonWriter.derived[ADTWithTypeA[B]]

    implicit def recursionTraitWithTypeBWriter[B: JsonWriter]
        : JsonObjectWriter[ADTWithTypeB[B]] =
      JsonWriter.derived[ADTWithTypeB[B]]

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[
      Int
    ]).asTokenList shouldBe obj(
      "a" -> 1,
      "b" -> obj(
        "a" -> 2
      )
    )
  }

  it should "derive writer that normally concatenates with other JsonWriter.derived's" in {
    implicit def recursionTraitWithTypeWriter[B: JsonWriter]
        : JsonWriter[ADTWithType[B]] = {
      val simpleJsonWriter =
        SimpleJsonObjectWriter[ADTWithType[B]].addField("clazz") {
          case _: ADTWithTypeA[B] => "ADTWithTypeA"
          case _: ADTWithTypeB[B] => "ADTWithTypeB"
        }
      simpleJsonWriter ++ JsonWriter.derived[ADTWithType[B]]
    }

    implicit def recursionTraitWithTypeAWriter[B: JsonWriter]
        : JsonObjectWriter[ADTWithTypeA[B]] =
      JsonWriter.derived[ADTWithTypeA[B]]

    implicit def recursionTraitWithTypeBWriter[B: JsonWriter]
        : JsonObjectWriter[ADTWithTypeB[B]] =
      JsonWriter.derived[ADTWithTypeB[B]]

    (ADTWithTypeB[Int](1, ADTWithTypeA[Int](2)): ADTWithType[
      Int
    ]).asTokenList shouldBe obj(
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
      implicit def recursionTraitWithTypeWriter[A: JsonWriter]: JsonWriter[ADTWithWrongType[A]] = JsonWriter.derived[ADTWithWrongType[A]]
      implicit def recursionTraitWithTypeAWriter[A: JsonWriter]: JsonWriter[ADTWithWrongType.ADTWithWrongTypeA[A]] = JsonWriter.derived[ADTWithWrongType.ADTWithWrongTypeA[A]]
      implicit def recursionTraitWithTypeAWriter[A: JsonWriter, B: JsonWriter]: JsonWriter[ADTWithWrongType.ADTWithWrongTypeB[A]] = JsonWriter.derived[ADTWithWrongType.ADTWithWrongTypeB[A]]
    """ shouldNot compile
  }

  it should "derive writer for simple sealed trait with hierarchy" in {
    implicit val caseClassWriter: JsonObjectWriter[CaseClass] =
      JsonWriter.derived[CaseClass]
    implicit val simpleClassWriter: JsonObjectWriter[SimpleClass] =
      JsonWriter.obj[SimpleClass].addField("b")(_.b)
    implicit val justObjectWriter: JsonObjectWriter[JustObject.type] =
      JsonWriter.obj.addField("type")(_ => "JustObject")
    implicit val subChildWriter: JsonObjectWriter[SubChild] =
      JsonWriter.derived[SubChild]

    implicit val sealedSubWriter: JsonObjectWriter[SimpleSealedTypeSub] =
      JsonWriter.derived[SimpleSealedTypeSub]
    implicit val sealedWriter: JsonWriter[SimpleSealedType] =
      JsonWriter.derived[SimpleSealedType]

    def write(simpleSealedType: SimpleSealedType): List[TokenNode] =
      simpleSealedType.asTokenList

    write(CaseClass(1)) shouldBe obj("__type" -> "CaseClass", "a" -> 1)
    write(SimpleClass(2)) shouldBe obj("__type" -> "SimpleClass", "b" -> 2)
    write(JustObject) shouldBe obj(
      "__type" -> "JustObject",
      "type" -> "JustObject"
    )
    write(SubChild(3)) shouldBe obj("__type" -> "SubChild", "c" -> 3)
  }

  it should "derive reader/writer for simple sealed trait with hierarchy with discriminator" in {
    implicit val caseClassWriter: JsonObjectWriter[CaseClass] =
      JsonWriter.derived[CaseClass]
    implicit val simpleClassWriter: JsonObjectWriter[SimpleClass] =
      JsonWriter.obj[SimpleClass].addField("b")(_.b)
    implicit val justObjectWriter: JsonObjectWriter[JustObject.type] =
      JsonWriter.obj
    implicit val subChildWriter: JsonObjectWriter[SubChild] =
      JsonWriter.derived[SubChild]
    given JsonReader[SimpleSealedType] = JsonReader.derived[SimpleSealedType]

    implicit val sealedWriter: JsonWriter[SimpleSealedType] =
      JsonWriter.derived[SimpleSealedType]

    def write(simpleSealedType: SimpleSealedType): List[TokenNode] =
      simpleSealedType.asTokenList

    write(CaseClass(1)) shouldBe obj("__type" -> "CaseClass", "a" -> 1)
    write(SimpleClass(2)) shouldBe obj("__type" -> "SimpleClass", "b" -> 2)
    write(JustObject) shouldBe obj("__type" -> "JustObject")
    write(SubChild(3)) shouldBe obj("__type" -> "SubChild", "c" -> 3)

    read[SimpleSealedType](
      obj("__type" -> "CaseClass", "a" -> 1)
    ) shouldBe CaseClass(1)
    read[SimpleSealedType](
      obj("__type" -> "SimpleClass", "b" -> 2)
    ) shouldBe SimpleClass(2)
    read[SimpleSealedType](obj("__type" -> "JustObject")) shouldBe JustObject
    read[SimpleSealedType](
      obj("__type" -> "SubChild", "c" -> 3)
    ) shouldBe SubChild(3)
  }

  it should "apply configuration for multiple case classes" in {

    inline given JsonConfiguration = JsonConfiguration.default
      .fieldStyle(FieldStyle.LowerSnakeCase)

    case class First(firstField: String) derives JsonWriter, JsonReader
    case class Second(secondField: String) derives JsonWriter, JsonReader

    val first = First("abc")
    val second = Second("bcd")
    val firstJson = obj("first_field" -> "abc")
    val secondJson = obj("second_field" -> "bcd")

    first.asTokenList shouldBe firstJson
    second.asTokenList shouldBe secondJson

    read[First](firstJson) shouldBe first
    read[Second](secondJson) shouldBe second
  }

  it should "apply configuration when derive product recursively" in {
    inline given JsonConfiguration = JsonConfiguration.default
      .fieldStyle(FieldStyle.LowerSnakeCase)

    case class Inner(innerField: String)
    case class Outer(outerField: Inner) derives JsonWriter, JsonReader

    val model = Outer(Inner("foo"))
    val json = obj("outer_field" -> obj("inner_field" -> "foo"))

    model.asTokenList shouldBe json
    read[Outer](json) shouldBe model
  }

  it should "apply configuration when derive sum recursively" in {
    inline given JsonConfiguration = JsonConfiguration.default
      .fieldStyle(FieldStyle.LowerSnakeCase)

    enum Choice(@selector val select: Int) derives JsonReader, JsonWriter:
      case First(firstField: Int) extends Choice(0)
      case Second(secondField: String) extends Choice(1)

    val first = Choice.First(1)
    val second = Choice.Second("foo")
    val firstJson = obj("select" -> 0, "first_field" -> 1)
    val secondJson = obj("select" -> 1, "second_field" -> "foo")

    first.asTokenList shouldBe firstJson
    second.asTokenList shouldBe secondJson

    read[Choice](firstJson) shouldBe first
    read[Choice](secondJson) shouldBe second
  }

  it should "select Writer/Reader configuration over JsonConfiguration as more specific" in {

    inline given WriterBuilder[Outer] =
      WriterBuilder[Outer].fieldStyle(FieldStyle.UpperCase)

    inline given ReaderBuilder[Outer] =
      ReaderBuilder[Outer].fieldStyle(FieldStyle.UpperCase)

    inline given JsonConfiguration =
      JsonConfiguration.default.fieldStyle(FieldStyle.LowerSnakeCase)

    case class Inner(innerField: String)
    case class Outer(outerField: Inner) derives JsonWriter, JsonReader

    val model = Outer(Inner("foo"))
    val json = obj("OUTERFIELD" -> obj("inner_field" -> "foo"))

    model.asTokenList shouldBe json
    read[Outer](json) shouldBe model

  }

  it should "respect strict JsonConfiguration setting" in {

    inline given JsonReader[SimpleType] = JsonReader.derived[SimpleType]

    inline given JsonConfiguration =
      JsonConfiguration.default.strict

    val json =
      obj("i" -> 5, "s" -> "foo", "d" -> 5.5, "anotherField" -> "bar")

    (the[ReaderError] thrownBy {
      read[SimpleType](
        json
      )
    }).getMessage shouldBe "Illegal json at '[ROOT]': unexpected field 'anotherField', expected one of 'i', 's', 'd'"

  }

}
