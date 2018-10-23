package tethys.derivation.impl.derivation

import tethys.JsonReader
import tethys.derivation.impl.builder.ReaderBuilderUtils
import tethys.derivation.impl.{BaseMacroDefinitions, CaseClassUtils}
import tethys.readers.{FieldName, JsonReaderDefaultValue}
import tethys.readers.tokens.TokenIterator

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.macros.blackbox

trait ReaderDerivation
  extends BaseMacroDefinitions
    with CaseClassUtils
    with DerivationUtils
    with ReaderBuilderUtils {
  val c: blackbox.Context
  import c.universe._

  private val fieldNameTerm = TermName(c.freshName("fieldName"))
  private val fieldNameType = tq"${weakTypeOf[FieldName]}"
  private val fieldNameTmp = TermName(c.freshName("fieldNameTmp"))

  private val tokenIteratorTerm = TermName(c.freshName("it"))
  private val tokenIteratorType = tq"${typeOf[TokenIterator]}"

  private val readerErrorCompanion = q"$readersPack.ReaderError"
  private val primitiveReadersCompanion = q"$readersPack.instances.PrimitiveReaders"

  private val jsonReaderDefaultValueType = tq"$readersPack.JsonReaderDefaultValue"
  private val jsonReaderType = tq"$tethysPack.JsonReader"
  private val somethingChanged = TermName(c.freshName("somethingChanged"))

  def deriveReader[A: WeakTypeTag]: Expr[JsonReader[A]] = {
    deriveReader(ReaderMacroDescription(Seq()))
  }

  def deriveReader[A: WeakTypeTag](description: ReaderMacroDescription): Expr[JsonReader[A]] = {
    val tpe = weakTypeOf[A]
    val classDef = caseClassDefinition(tpe)
    implicit val context: ReaderContext = new ReaderContext

    val name = TermName(c.freshName("name"))

    val definitions = classDef.fields.map(field => FieldDefinitions(field.name, field.tpe))
    val syntheticDefinitions = extractSyntheticDefintions(description, classDef)
    val transformedDefinitions = transformDefinitions(description, definitions)

    val allDefinitions = transformedDefinitions ++ syntheticDefinitions
    allDefinitions.foreach(context.addDefinition)

    val vars = allDefinitions.flatMap(_.definitions)
    val cases = allDefinitions.flatMap(_.fieldCase) :+ cq"_ => $tokenIteratorTerm.skipExpression()"
    val isAllInitialized: Tree = {
      val trees = transformedDefinitions.map(d => q"${d.isInitialized}")
      if(trees.size < 2) trees.headOption.getOrElse(q"true")
      else trees.reduceLeft[Tree] {
        case (left, right) => q"$left && $right"
      }
    }

    val defaultValues =
      q"""
          ..${syntheticDefinitions.flatMap(_.defaultValueExtraction)}

          ..${transformedDefinitions.filter(_.extractionType == Direct).flatMap(_.defaultValueExtraction)}
       """

    val transformations = {
      if(!transformedDefinitions.exists(_.extractionType != Direct)) EmptyTree
      else
        q"""
           var $somethingChanged = true
           while($somethingChanged) {
             $somethingChanged = false
             ..${transformedDefinitions.filter(_.extractionType != Direct).map(_.transformation)}
           }

           ..${transformedDefinitions.filter(_.extractionType == FromFields).map(_.defaultValueExtraction)}
         """
    }

    val uninitializedFieldsReason: Tree = {
      val uninitializedFields = TermName(c.freshName("uninitializedFields"))
      val fields = transformedDefinitions.map(d => q"""if(!${d.isInitialized}) $uninitializedFields += ${d.name} """)
      q"""
         val $uninitializedFields = scala.collection.mutable.ArrayBuffer.empty[String]
         ..$fields
         "Can not extract fields " + $uninitializedFields.mkString("'", "', '", "'")
       """
    }

    c.Expr[JsonReader[A]] {
      c.untypecheck {
        q"""
           new $jsonReaderType[$tpe] {
              ..${context.defaultValues}

              ${provideThisReaderImplicit(tpe)}

              ..${context.readers}

              ..${context.functions}

              override def read($tokenIteratorTerm: $tokenIteratorType)(implicit $fieldNameTerm: $fieldNameType): $tpe = {
                if(!$tokenIteratorTerm.currentToken().isObjectStart) $readerErrorCompanion.wrongJson("Expected object start but found: "  + $tokenIteratorTerm.currentToken().toString)
                else {
                  val $fieldNameTmp = $fieldNameTerm
                  $tokenIteratorTerm.nextToken()
                  ..$vars

                  while(!$tokenIteratorTerm.currentToken().isObjectEnd) {
                    val $name = $tokenIteratorTerm.fieldName()
                    $tokenIteratorTerm.nextToken()
                    implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
                    $name match { case ..$cases }
                  }
                  $tokenIteratorTerm.nextToken()

                  $defaultValues

                  $transformations

                  if(!($isAllInitialized)) $readerErrorCompanion.wrongJson($uninitializedFieldsReason)
                  else new ${weakTypeOf[A]}(..${transformedDefinitions.map(_.value)})
                }
              }
           }: $jsonReaderType[$tpe]
        """
      }
    }
  }

  private def extractSyntheticDefintions(description: ReaderMacroDescription, classDef: CaseClassDefinition)
                                        (implicit context: ReaderContext): Seq[FieldDefinitions] = {
    val names = classDef.fields.map(_.name).toSet

    val fields: Seq[Field] = description.operations.flatMap {
      case _: ReaderMacroOperation.ExtractFieldAs =>
        Seq.empty

      case ReaderMacroOperation.ExtractFieldValue(_, fs, _) =>
        fs.filterNot(f => names(f.name))

      case ReaderMacroOperation.ExtractFieldReader(_, fs, _) =>
        fs.filterNot(f => names(f.name))
    }

    fields.map(f => f.name -> f.tpe).toMap.toSeq.map {
      case (name, tpe) => FieldDefinitions(name, tpe)
    }
  }

  private def transformDefinitions(description: ReaderMacroDescription, definitions: Seq[FieldDefinitions])
                                  (implicit context: ReaderContext): Seq[FieldDefinitions] = {
    val fieldDesctiptions = description.operations.map(f => f.field -> f).toMap

    definitions.map(d => fieldDesctiptions.get(d.name).fold(d) {
      case op: ReaderMacroOperation.ExtractFieldAs =>
        d.copy(extractionType = Direct, transformer = Some(op))

      case op: ReaderMacroOperation.ExtractFieldValue =>
        d.copy(extractionType = FromFields, transformer = Some(op))

      case op: ReaderMacroOperation.ExtractFieldReader=>
        d.copy(extractionType = FromExtractedReader, transformer = Some(op))
    })
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
    private val funs: mutable.ArrayBuffer[(TermName, Tree)] = mutable.ArrayBuffer[(TermName, Tree)]()
    private val definitions: mutable.Map[String, FieldDefinitions] = mutable.Map[String, FieldDefinitions]()

    def provideReader(tpe: Type): TermName = {
      readersMapping.getOrElseUpdate(unwrapType(tpe), TermName(c.freshName("reader")))
    }

    def provideDefaultValue(tpe: Type): TermName = {
      defaultValuesMapping.getOrElseUpdate(unwrapType(tpe), TermName(c.freshName("defaultValue")))
    }

    def registerFunction(fun: Tree): TermName = {
      funs.find(_._2 eq fun) match {
        case Some((name, _)) => name
        case _ =>
          val name = TermName(c.freshName("fun"))
          funs += name -> fun
          name
      }
    }

    def addDefinition(d: FieldDefinitions): Unit = definitions += d.name -> d

    def definition(name: String): FieldDefinitions = definitions(name)

    def readers: Seq[Tree] = readersMapping.map {
      case (tpe, name) if tpe =:= typeOf[Short] =>
        q"private[this] val $name = $primitiveReadersCompanion.ShortJsonReader"
      case (tpe, name) if tpe =:= typeOf[Int] =>
        q"private[this] val $name = $primitiveReadersCompanion.IntJsonReader"
      case (tpe, name) if tpe =:= typeOf[Long] =>
        q"private[this] val $name = $primitiveReadersCompanion.LongJsonReader"
      case (tpe, name) if tpe =:= typeOf[Float] =>
        q"private[this] val $name = $primitiveReadersCompanion.FloatJsonReader"
      case (tpe, name) if tpe =:= typeOf[Double] =>
        q"private[this] val $name = $primitiveReadersCompanion.DoubleJsonReader"
      case (tpe, name) if tpe =:= typeOf[Boolean] =>
        q"private[this] val $name = $primitiveReadersCompanion.BooleanJsonReader"

      case (tpe, name) =>
        q"private[this] lazy val $name = implicitly[$jsonReaderType[$tpe]]"
    }.toSeq

    def defaultValues: Seq[Tree] = defaultValuesMapping.map {
      case (tpe, name) =>
        q"private[this] var $name: $tpe = _"
    }.toSeq

    def functions: Seq[Tree] = funs.toList.map {
      case (name, fun) => q"private[this] val $name = $fun"
    }

    @tailrec
    private def unwrapType(tpe: Type): Type = tpe match {
      case ConstantType(const) => unwrapType(const.tpe)
      case _ => tpe
    }
  }

  sealed trait ExtractionType
  case object Direct extends ExtractionType
  case object FromFields extends ExtractionType
  case object FromExtractedReader extends ExtractionType

  protected case class FieldDefinitions(name: String,
                                        tpe: Type,
                                        extractionType: ExtractionType = Direct,
                                        transformer: Option[ReaderMacroOperation] = None)
                                       (implicit readerContext: ReaderContext) {
    lazy val value: TermName = TermName(c.freshName(name + "Field"))
    lazy val valueTree: TermName = TermName(c.freshName(name + "FieldTree"))
    lazy val isInitialized: TermName = TermName(c.freshName(name + "FieldInitialized"))
    lazy val defaultValue: TermName = TermName(c.freshName("defaultValue"))

    def definitions: List[Tree] = {
      val defs = q"""
         {
           var $value: $tpe = ${readerContext.provideDefaultValue(tpe)}
           var $isInitialized: Boolean = false
         }
       """.children

      if(extractionType == FromExtractedReader) defs :+ q"var $valueTree: $tokenIteratorType = null"
      else defs
    }

    def defaultValueExtraction: Option[Tree] = {
      val (valueTpe, valueFun) = transformer match {
        case Some(op: ReaderMacroOperation.ExtractFieldAs) =>
          (op.as, (defaultValue: Tree) => q"${readerContext.registerFunction(op.fun)}.apply($defaultValue.asInstanceOf[${op.as}])")
        case _ =>
          (tpe, (defaultValue: Tree) => q"$defaultValue.asInstanceOf[$tpe]")
      }
      extractDefaultValue(valueTpe).map { defaultValue =>
        q"""
           if(!$isInitialized) {
              $value = ${valueFun(defaultValue)}
              $isInitialized = true
           }
         """
      }
    }

    def fieldCase: Option[CaseDef] = transformer match {
      case Some(op: ReaderMacroOperation.ExtractFieldAs) =>
        val caseDef = cq"""
          $name =>
            $value = ${readerContext.registerFunction(op.fun)}.apply(${readerContext.provideReader(op.as)}.read($tokenIteratorTerm))
            $isInitialized = true
        """
        Some(caseDef)

      case Some(op: ReaderMacroOperation.ExtractFieldReader) =>
        val caseDef = cq"""
          $name =>
            $valueTree = $tokenIteratorTerm.collectExpression()
        """
        Some(caseDef)

      case None =>
        val caseDef = cq"""
          $name =>
            $value = ${readerContext.provideReader(tpe)}.read($tokenIteratorTerm)
            $isInitialized = true
        """
        Some(caseDef)

      case _ =>
        None
    }

    def transformation: Tree = transformer.fold(EmptyTree) {
      case op: ReaderMacroOperation.ExtractFieldValue =>
        val canTransform = op.from.map(f => readerContext.definition(f.name).isInitialized)
          .foldLeft[Tree](q"!$isInitialized") {
          case (current, next) => q"$current && $next"
        }

        val args = op.from.map(f => readerContext.definition(f.name).value)

        q"""
           if($canTransform) {
              implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
              $value = ${readerContext.registerFunction(op.fun)}.apply(..$args)
              $isInitialized = true
              $somethingChanged = true
           }
         """

      case op: ReaderMacroOperation.ExtractFieldReader =>
        val canTransform = op.from.map(f => readerContext.definition(f.name).isInitialized)
          .foldLeft[Tree](q"!$isInitialized") {
            case (current, next) => q"$current && $next"
          }

        val args = op.from.map(f => readerContext.definition(f.name).value)
        val reader = TermName(c.freshName(name + "Reader"))
        extractDefaultValue(tpe) match {
          case Some(defaultValue) =>
            q"""
             if($canTransform) {
                val $reader = ${readerContext.registerFunction(op.fun)}.apply(..$args)
                if($valueTree != null) {
                  implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
                  $value = $reader.read($valueTree)
                  $isInitialized = true
                  $somethingChanged = true
                } else {
                  $value = $defaultValue.asInstanceOf[$tpe]
                  $isInitialized = true
                }
             }
           """
          case None =>
            q"""
             if($canTransform && $valueTree != null) {
                val $reader = ${readerContext.registerFunction(op.fun)}.apply(..$args)
                implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
                $value = $reader.read($valueTree)
                $isInitialized = true
                $somethingChanged = true
             }
           """
        }


      case _ =>
        EmptyTree
    }

    def extractDefaultValue(tpe: Type): Option[Tree] = {
      c.typecheck(q"implicitly[$jsonReaderDefaultValueType[$tpe]]") match {
        case q"$_.implicitly[$_]($defaultValue)" =>
          val mbValue = defaultValue.tpe.typeSymbol.annotations.map(_.tree).collectFirst {
            case q"new $clazz(${value: Tree})" if clazz.tpe =:= typeOf[JsonReaderDefaultValue.ReaderDefaultValue] =>
              value
          }

          mbValue match {
            case None =>
              abort(s"JsonReaderDefaultValue '${defaultValue.tpe}' is not annotated with 'ReaderDefaultValue'")
            case Some(q"null") =>
              None
            case Some(value) =>
              Some(value)
          }

      }
    }

  }
}
