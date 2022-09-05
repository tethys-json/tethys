package tethys.derivation

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import tethys._
import tethys.writers.tokens.SimpleTokenWriter._
import tethys.commons.TokenNode._
import tethys.derivation.builder.WriterBuilder
import tethys.derivation.semiauto._

class WriterRenamingSyntaxTest extends AnyFlatSpec with Matchers {
  behavior of "renaming syntax"
  it should "rename field on 'rename'" in {
    implicit lazy val writer: JsonWriter[D] = jsonWriter[D] {
      describe {
        WriterBuilder[D]
          .rename(_.a)("b")
      }
    }

    D(1).asTokenList shouldBe obj(
      "b" -> 1
    )
  }

  it should "rename field on update" in {
    implicit lazy val writer: JsonWriter[D] = jsonWriter[D] {
      describe {
        WriterBuilder[D]
          .update(_.a).withRename("b")(_.toString)
      }
    }

    D(1).asTokenList shouldBe obj(
      "b" -> "1"
    )
  }

  it should "rename field on update with free variable" in {
    def freeVariableRenaming(name: String): JsonWriter[D] = jsonWriter[D] {
      describe {
        WriterBuilder[D]
          .update(_.a).withRename(name)(_.toString)
      }
    }

    D(a = 1).asTokenList(freeVariableRenaming("b")) shouldBe obj(
      "b" -> "1"
    )

    D(a = 1).asTokenList(freeVariableRenaming("c")) shouldBe obj(
      "c" -> "1"
    )
  }

  it should "rename field on update from root" in {
    implicit lazy val writer: JsonWriter[D] = jsonWriter[D] {
      describe {
        WriterBuilder[D]
          .update(_.a).withRename("b").fromRoot(d => d.a * 2)
      }
    }

    D(1).asTokenList shouldBe obj(
      "b" -> 2
    )
  }

  it should "rename field on updatePartial" in {
    implicit lazy val writer: JsonWriter[D] = jsonWriter[D] {
      describe {
        WriterBuilder[D].updatePartial(_.a).withRename("b") {
          case 1 => "uno"
          case i => i
        }
      }
    }

    D(1).asTokenList shouldBe obj(
      "b" -> "uno"
    )

    D(2).asTokenList shouldBe obj(
      "b" -> 2
    )
  }

  it should "rename field on updatePartial from root" in {
    implicit lazy val writer: JsonWriter[D] = jsonWriter[D] {
      describe {
        WriterBuilder[D].updatePartial(_.a).withRename("b").fromRoot {
          case D(1) => "uno"
          case D(i) => i * 2
        }
      }
    }

    D(1).asTokenList shouldBe obj(
      "b" -> "uno"
    )

    D(2).asTokenList shouldBe obj(
      "b" -> 4
    )
  }
}
