package tethys.derivation.impl.derivation

import tethys.JsonReader
import tethys.derivation.impl.{BaseMacroDefinitions, CaseClassUtils}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

trait ReaderDerivation
  extends BaseMacroDefinitions
    with CaseClassUtils
    with DerivationUtils {
  val c: blackbox.Context
  import c.universe._

  private val fieldNameTerm = TermName(c.freshName("fieldName"))
  private val fieldNameType = tq"${weakTypeOf[FieldName]}"

  private val tokenIteratorTerm = TermName(c.freshName("it"))
  private val tokenIteratorType = tq"${typeOf[TokenIterator]}"

  private val errorTerm = TermName(c.freshName("error"))
  private val readerErrorCompanion = q"$readersPack.ReaderError"

  private val jsonReaderType = tq"$tethysPack.JsonReader"

  def deriveReader[A: WeakTypeTag]: Expr[JsonReader[A]] = {
    val tpe = weakTypeOf[A]
    val classDef = caseClassDefinition(tpe)
    implicit val context: ReaderContext = new ReaderContext

    val nameOpt = TermName(c.freshName("nameOpt"))
    val name = TermName(c.freshName("name"))
    val fieldNameTmp = TermName(c.freshName("fieldNameTmp"))

    val definitions = classDef.fields.map(field => new FieldDefinitions(field.name, field.tpe))

    val vars = definitions.flatMap(_.definitions)
    val cases = definitions.map(_.fieldCase) :+ cq"_ => $tokenIteratorTerm.skipExpression()"
    val isAllInitialized: Tree = {
      val trees = definitions.map(d => q"${d.isInitialized}")
      if(trees.size < 2) trees.headOption.getOrElse(q"true")
      else trees.reduceLeft[Tree] {
        case (left, right) => q"$left && $right"
      }
    }

    val resultType = weakTypeOf[Either[ReaderError, A]]
    c.Expr[JsonReader[A]] {
      c.untypecheck {
        q"""
           new $jsonReaderType[$tpe] {
              ..${context.defaultValues}

              ${provideThisReaderImplicit(tpe)}

              ..${context.readers}

              override def read($tokenIteratorTerm: $tokenIteratorType)(implicit $fieldNameTerm: $fieldNameType): $resultType = {
                if(!$tokenIteratorTerm.currentToken().isObjectStart) $readerErrorCompanion.wrongType[$tpe]
                else {
                  val $fieldNameTmp = $fieldNameTerm
                  $tokenIteratorTerm.nextToken()
                  var $errorTerm: ${weakTypeOf[Either[ReaderError, _]]} = null
                  ..$vars

                  while($errorTerm == null && !$tokenIteratorTerm.currentToken().isObjectEnd) {
                    val $nameOpt = $tokenIteratorTerm.fieldName()
                    if($nameOpt.isEmpty) {
                      $errorTerm = $readerErrorCompanion.wrongJson
                    } else {
                      $tokenIteratorTerm.nextToken()
                      val $name = $nameOpt.get
                      implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
                      $name match { case ..$cases }
                    }
                  }
                  $tokenIteratorTerm.nextToken()

                  if($errorTerm != null) $errorTerm.asInstanceOf[$resultType]
                  else if(!($isAllInitialized)) $readerErrorCompanion.wrongJson
                  else Right(new ${weakTypeOf[A]}(..${definitions.map(_.value)}))
                }
              }
           }: $jsonReaderType[$tpe]
        """
      }
    }
  }

  private def provideThisReaderImplicit(tpe: Type): Tree = {
    c.typecheck(q"implicitly[$jsonReaderType[$tpe]]", silent = true) match {
      case EmptyTree =>
        val thisWriterTerm = TermName(c.freshName("thisWriter"))
        q"implicit private[this] def $thisWriterTerm: $jsonReaderType[$tpe] = this"
      case _ => EmptyTree
    }
  }

  protected class ReaderContext {
    private val readersMapping: mutable.Map[Type, TermName] = mutable.Map[Type, TermName]()
    private val defaultValuesMapping: mutable.Map[Type, TermName] = mutable.Map[Type, TermName]()

    def provideReader(tpe: Type): TermName = {
      readersMapping.getOrElseUpdate(unwrapType(tpe), TermName(c.freshName("reader")))
    }

    def provideDefaultValue(tpe: Type): TermName = {
      defaultValuesMapping.getOrElseUpdate(unwrapType(tpe), TermName(c.freshName("defaultValue")))
    }

    def readers: Seq[Tree] = readersMapping.map {
      case (tpe, name) =>
        q"private[this] lazy val $name = implicitly[$jsonReaderType[$tpe]]"
    }.toSeq

    def defaultValues: Seq[Tree] = defaultValuesMapping.map {
      case (tpe, name) =>
        q"private[this] var $name: $tpe = _"
    }.toSeq

    @tailrec
    private def unwrapType(tpe: Type): Type = tpe match {
      case ConstantType(const) => unwrapType(const.tpe)
      case _ => tpe
    }
  }

  protected class FieldDefinitions(name: String, tpe: Type)(implicit readerContext: ReaderContext) {
    val value: TermName = TermName(c.freshName(name + "Field"))
    val isInitialized: TermName = TermName(c.freshName(name + "FieldInitialized"))
    val defaultValue: TermName = TermName(c.freshName("defaultValue"))

    def definitions: List[Tree] = {
      q"""
         {
           var $value: $tpe = ${readerContext.provideDefaultValue(tpe)}
           var $isInitialized: Boolean = false
           ${readerContext.provideReader(tpe)}.defaultValue.foreach { ($defaultValue: $tpe) =>
              $value = $defaultValue
              $isInitialized = true
           }
         }
       """.children
    }

    def fieldCase: CaseDef = {
      val right = TermName(c.freshName(name + "Value"))
      val left = TermName(c.freshName(name + "Error"))
      cq"""
          $name =>
            ${readerContext.provideReader(tpe)}.read($tokenIteratorTerm) match {
              case Right($right) =>
                $value = $right
                $isInitialized = true

              case $left =>
                $errorTerm = $left

            }
        """
    }
  }
}
