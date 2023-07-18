package tethys.derivation.impl

import scala.reflect.macros.blackbox

/**
  * Created by eld0727 on 22.04.17.
  */
trait MacroUtils extends BaseMacroDefinitions
    with CaseClassUtils
    with LoggingUtils {
  val c: blackbox.Context
  import c.universe._

  def eval[T](expr: Expr[T]): Option[T] = {
    util.Try(c.eval(c.Expr[T](c.untypecheck(expr.tree)))).toOption
  }

  case class SelectChain(chain: Seq[String])

  implicit lazy val selectChainUnliftable: Unliftable[SelectChain] = Unliftable[SelectChain] {
    case Ident(name) => SelectChain(Seq(name.decodedName.toString))
    case select: Select =>
      def selectAllNames(s: Tree): Seq[String] = s match {
        case Select(rest, name) => selectAllNames(rest) :+ name.decodedName.toString
        case Ident(name) => Seq(name.decodedName.toString)
      }

      SelectChain(selectAllNames(select))
  }


  case class BuilderField(name: String, tpe: Type)

  implicit lazy val builderFieldUnliftable: Unliftable[BuilderField] = Unliftable[BuilderField] {
    case lambda@q"((${ValDef(_, name, _, _)}) => ${b: SelectChain})"
      if b.chain.size == 2 && name.decodedName.toString == b.chain.head =>
      val tpe = lambda match {
        case q"($_ => ${body: Tree})" => body.tpe
      }
      BuilderField(b.chain(1), tpe)
  }

  object Untyped {
    def unapply(arg: Tree): Option[Tree] = arg match {
      case Typed(t, _) => Untyped.unapply(t)
      case _ => Some(arg)
    }
  }

  implicit lazy val optionTreeUnliftable: Unliftable[Option[Tree]] = Unliftable[Option[Tree]] {
    case q"$col.Some.apply[$_](${res: Tree})" => Some(res)
    case q"$col.None" => None
  }
}
