package tethys.derivation.impl.derivation

import tethys.JsonReader
import tethys.derivation.builder.FieldStyle.StyleReference
import tethys.derivation.builder.{FieldStyle, ReaderDerivationConfig}
import tethys.derivation.impl.builder.{ReaderBuilderUtils, ReaderDescriptionCommons}
import tethys.derivation.impl.{BaseMacroDefinitions, CaseClassUtils}
import tethys.readers.tokens.TokenIterator
import tethys.readers.{FieldName, JsonReaderDefaultValue}

import scala.reflect.macros.blackbox

trait ReaderDerivation
  extends BaseMacroDefinitions
    with CaseClassUtils
    with DerivationUtils
    with ReaderDescriptionCommons {
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



  private sealed trait ReaderField {
    def value: TermName
  }
  private case class SimpleField(name: String,
                                 tpe: Type,
                                 jsonName: String,
                                 value: TermName,
                                 isInitialized: TermName) extends ReaderField

  private case class ExtractedField(name: String,
                                    tpe: Type,
                                    functionName: TermName,
                                    args: List[FunctionArgument],
                                    body: Tree,
                                    value: TermName,
                                    isInitialized: TermName) extends ReaderField

  private case class FromExtractedReader(name: String,
                                         tpe: Type,
                                         jsonName: String,
                                         functionName: TermName,
                                         args: List[FunctionArgument],
                                         body: Tree,
                                         value: TermName,
                                         isInitialized: TermName,
                                         tempIterator: TermName) extends ReaderField

  private case class FunctionArgument(field: Field, value: TermName, isInitialized: TermName)

  def deriveReader[A: WeakTypeTag]: Expr[JsonReader[A]] = {
    deriveReader(ReaderMacroDescription(emptyReaderConfig, Seq()))
  }

  def deriveReader[A: WeakTypeTag](description: ReaderMacroDescription): Expr[JsonReader[A]] = {
    val tpe = weakTypeOf[A]
    val classDef = caseClassDefinition(tpe)
    val config = scala.util.Try(c.eval(description.config)).getOrElse(c.eval(description.config))

    val readerFields = applyFieldStyle(config.fieldStyle)
      .andThen(applyOperations(description.operations))
      .apply(classDef.fields.map { field =>
        SimpleField(
          name = field.name,
          tpe = field.tpe,
          jsonName = field.name,
          value = TermName(c.freshName(field.name + "Value")),
          isInitialized = TermName(c.freshName(field.name + "Init"))
        )
      })

    val (typeReaders, readerTrees) = allocateReaders(readerFields)
    val (typeDefaultValues, defaultValuesTrees) = allocateDefaultValues(readerFields)
    val variablesTrees = allocateVariables(readerFields, typeDefaultValues)
    val functionsTrees = allocateFunctions(readerFields)
    val cases = allocateCases(config.isStrict, readerFields, typeReaders)
    val rawPostProcessing = allocateRawFieldsPostProcessing(readerFields)
    val transformations = allocateTransformationsLoop(readerFields)

    val name = TermName(c.freshName("name"))

    c.Expr[JsonReader[A]] {
      c.untypecheck {
        q"""
           new $jsonReaderType[$tpe] {
              ..$defaultValuesTrees
              ${provideThisReaderImplicit(tpe)}
              ..$readerTrees
              ..$functionsTrees

              override def read($tokenIteratorTerm: $tokenIteratorType)(implicit $fieldNameTerm: $fieldNameType): $tpe = {
                if(!$tokenIteratorTerm.currentToken().isObjectStart) $readerErrorCompanion.wrongJson("Expected object start but found: "  + $tokenIteratorTerm.currentToken().toString)
                else {
                  val $fieldNameTmp = $fieldNameTerm
                  $tokenIteratorTerm.nextToken()
                  ..$variablesTrees

                  while(!$tokenIteratorTerm.currentToken().isObjectEnd) {
                    val $name = $tokenIteratorTerm.fieldName()
                    $tokenIteratorTerm.nextToken()
                    implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
                    $name match { case ..$cases }
                  }
                  $tokenIteratorTerm.nextToken()

                  $rawPostProcessing

                  $transformations

                  new ${weakTypeOf[A]}(..${readerFields.map(_.value)})
                }
              }
           }: $jsonReaderType[$tpe]
        """
      }
    }
  }

  private def applyFieldStyle(fieldStyle: Option[FieldStyle]): List[SimpleField] => List[SimpleField] = readerFields => {
    fieldStyle.fold(readerFields) { style =>
      readerFields.map(f => f.copy(jsonName = style.applyStyle(f.jsonName)))
    }
  }

  private def applyOperations(operations: Seq[ReaderMacroOperation]): List[ReaderField] => List[ReaderField] = readerFields => {
    def mapField(fields: List[ReaderField], name: String)(f: SimpleField => ReaderField): List[ReaderField] = {
      fields.map {
        case field: SimpleField if field.name == name => f(field)
        case field => field
      }
    }

    def buildArgument(field: Field, readerFields: List[ReaderField]): FunctionArgument = {
      field match {
        case Field.ClassField(name, _) =>
          readerFields.collectFirst {
            case f: SimpleField if f.name == name =>
              FunctionArgument(field, f.value, f.isInitialized)
            case f: ExtractedField if f.name == name =>
              FunctionArgument(field, f.value, f.isInitialized)
            case f: FromExtractedReader if f.name == name =>
              FunctionArgument(field, f.value, f.isInitialized)
          }.head
        case Field.RawField(name, tpe) =>
          val possibleArg = readerFields.flatMap {
            case f: SimpleField if f.jsonName == name && f.tpe =:= tpe =>
              List(FunctionArgument(field, f.value, f.isInitialized))
            case f: ExtractedField =>
              f.args.collectFirst {
                case arg@FunctionArgument(rf: Field.RawField, _, _) if rf.name == name && rf.tpe =:= tpe =>
                  arg
              }
            case f: FromExtractedReader =>
              f.args.collectFirst {
                case arg@FunctionArgument(rf: Field.RawField, _, _) if rf.name == name && rf.tpe =:= tpe =>
                  arg
              }
            case _ =>
              List.empty[FunctionArgument]
          }

          possibleArg.headOption.getOrElse(FunctionArgument(
            field = Field.RawField(name, tpe),
            value = TermName(c.freshName(name + "Value")),
            isInitialized = TermName(c.freshName(name + "Init"))
          ))
      }
    }

    operations.foldLeft(readerFields) {
      case (fields, operation) =>
        operation match {
          case ReaderMacroOperation.ExtractFieldAs(field, tpe, as, fun) =>
            mapField(fields, field)(f => ExtractedField(
              name = field,
              tpe = tpe,
              functionName = TermName(c.freshName(field + "Fun")),
              args = List(FunctionArgument(
                field = Field.RawField(f.jsonName, as),
                value = TermName(c.freshName(field + "Value")),
                isInitialized = TermName(c.freshName(field + "Init"))
              )),
              body = fun,
              value = f.value,
              isInitialized = f.isInitialized
            ))

          case ReaderMacroOperation.ExtractFieldValue(field, from, fun) =>
            mapField(fields, field)(f => ExtractedField(
              name = field,
              tpe = f.tpe,
              functionName = TermName(c.freshName(field + "Fun")),
              args = from.toList.map(buildArgument(_, fields)),
              body = fun,
              value = f.value,
              isInitialized = f.isInitialized
            ))
          case ReaderMacroOperation.ExtractFieldReader(field, from, fun) =>
            mapField(fields, field)(f => FromExtractedReader(
              name = field,
              tpe = f.tpe,
              jsonName = f.jsonName,
              functionName = TermName(c.freshName(field + "JsonFun")),
              args = from.toList.map(buildArgument(_, fields)),
              body = fun,
              value = f.value,
              isInitialized = f.isInitialized,
              tempIterator = TermName(c.freshName(field + "TmpIter"))
            ))
        }
    }
  }

  private def allocateReaders(readerFields: List[ReaderField]): (List[(Type, TermName)], List[Tree]) = {
    val jsonTypes = readerFields.flatMap {
      case f: SimpleField =>
        List(f.tpe)
      case f: ExtractedField =>
        f.args.map(_.field.tpe)
      case f: FromExtractedReader =>
        f.args.map(_.field.tpe)
    }

    jsonTypes.foldLeft((List[(Type, TermName)](), List[Tree]())) {
      case ((types, trees), tpe) if !types.exists(_._1 =:= tpe) =>
        val term = TermName(c.freshName())
        val reader = {
          if (tpe =:= typeOf[Short]) q"private[this] val $term = $primitiveReadersCompanion.ShortJsonReader"
          else if (tpe =:= typeOf[Int]) q"private[this] val $term = $primitiveReadersCompanion.IntJsonReader"
          else if (tpe =:= typeOf[Long]) q"private[this] val $term = $primitiveReadersCompanion.LongJsonReader"
          else if (tpe =:= typeOf[Float]) q"private[this] val $term = $primitiveReadersCompanion.FloatJsonReader"
          else if (tpe =:= typeOf[Double]) q"private[this] val $term = $primitiveReadersCompanion.DoubleJsonReader"
          else if (tpe =:= typeOf[Boolean]) q"private[this] val $term = $primitiveReadersCompanion.BooleanJsonReader"
          else q"private[this] lazy val $term = implicitly[$jsonReaderType[$tpe]]"
        }
        (tpe -> term :: types, reader :: trees)

      case (res, _) => res
    }
  }

  private def allocateDefaultValues(readerFields: List[ReaderField]): (List[(Type, TermName)], List[Tree]) = {
    val allTypes = readerFields.flatMap {
      case f: SimpleField =>
        List(f.tpe)
      case f: ExtractedField =>
        f.tpe :: f.args.map(_.field.tpe)
      case f: FromExtractedReader =>
        f.tpe :: f.args.map(_.field.tpe)
    }
    allTypes.foldLeft((List[(Type, TermName)](), List[Tree]())) {
      case ((types, trees), tpe) if !types.exists(_._1 =:= tpe) =>
        val term = TermName(c.freshName())
        val default = q"private[this] var $term: $tpe = _"

        (tpe -> term :: types, default :: trees)

      case (res, _) => res
    }
  }

  private def allocateVariables(readerFields: List[ReaderField], typeDefaultValues: List[(Type, TermName)]): List[Tree] = {
    val possibleValues: List[(TermName, Type)] = readerFields.flatMap {
      case f: SimpleField =>
        List(f.value -> f.tpe)
      case f: ExtractedField =>
        (f.value, f.tpe) :: f.args.map(arg => arg.value -> arg.field.tpe)
      case f: FromExtractedReader =>
        (f.value, f.tpe) :: f.args.map(arg => arg.value -> arg.field.tpe)
    }

    val (_, values) = possibleValues.foldLeft(List[TermName](), List[Tree]()) {
      case ((allocated, trees), (value, tpe)) if !allocated.contains(value) =>
        val tree = q"var $value: $tpe = ${typeDefaultValues.find(_._1 =:= tpe).get._2}"
        (value :: allocated, tree :: trees)

      case (res, _) => res
    }

    val inits = readerFields
      .flatMap {
        case f: SimpleField =>
          List(f.isInitialized)
        case f: ExtractedField =>
          f.isInitialized :: f.args.map(_.isInitialized)
        case f: FromExtractedReader =>
          f.isInitialized :: f.args.map(_.isInitialized)
      }
      .distinct
      .map(term => q"var $term: Boolean = false")

    val tempIterators = readerFields.collect {
      case f: FromExtractedReader =>
        q"var ${f.tempIterator}: $tokenIteratorType = null"
    }

    values ::: inits ::: tempIterators
  }

  private def allocateFunctions(readerFields: List[ReaderField]): List[Tree] = readerFields.collect {
    case f: ExtractedField =>
      q"private[this] val ${f.functionName} = ${f.body}"
    case f: FromExtractedReader =>
      q"private[this] val ${f.functionName} = ${f.body}"
  }


  private def allocateCases(isStrict: Boolean, readerFields: List[ReaderField], readers: List[(Type, TermName)]): List[CaseDef] = {
    sealed trait FieldDef {
      def jsonName: String
    }
    case class SimpleFieldDef(jsonName: String, reader: TermName, value: TermName, isInitialized: TermName) extends FieldDef
    case class CustomReaderFieldDef(jsonName: String, tempIterator: TermName) extends FieldDef

    def findReader(tpe: Type) = readers.find(_._1 =:= tpe).get._2

    val fieldDefs: List[FieldDef] = readerFields.flatMap {
      case f: SimpleField =>
        List(SimpleFieldDef(f.jsonName, findReader(f.tpe), f.value, f.isInitialized))
      case f: ExtractedField =>
        f.args.collect {
          case FunctionArgument(Field.RawField(jsonName, tpe), value, isInitialized) =>
            SimpleFieldDef(jsonName, findReader(tpe), value, isInitialized)
        }
      case f: FromExtractedReader =>
        CustomReaderFieldDef(f.jsonName, f.tempIterator) :: f.args.collect {
          case FunctionArgument(Field.RawField(jsonName, tpe), value, isInitialized) =>
            SimpleFieldDef(jsonName, findReader(tpe), value, isInitialized)
        }
    }

    val gropedDefs = fieldDefs.distinct.groupBy(_.jsonName).toList.sortBy(f => fieldDefs.indexWhere(_.jsonName == f._1))

    val res = gropedDefs.map {
      case (jsonName, List(fieldDef)) =>
        fieldDef match {
          case SimpleFieldDef(_, reader, value, isInitialized) =>
            cq"""
              $jsonName =>
                $value = $reader.read($tokenIteratorTerm)
                $isInitialized = true
             """
          case CustomReaderFieldDef(_, tempIterator) =>
            cq"$jsonName => $tempIterator = $tokenIteratorTerm.collectExpression()"
        }
      case (jsonName, defs) =>
        val fieldIterator = TermName(c.freshName(jsonName + "Iter"))
        val body = q"val $fieldIterator = $tokenIteratorTerm.collectExpression()" :: defs.flatMap {
          case SimpleFieldDef(_, reader, value, isInitialized) =>
            q"$value = $reader.read($fieldIterator.copy())" ::
              q"$isInitialized = true" :: Nil
          case CustomReaderFieldDef(_, tempIterator) =>
            q"$tempIterator = $fieldIterator.copy()" :: Nil
        }

        cq"""
            $jsonName => ..$body
          """
    }

    val defaultCase = {
      if(isStrict) {
        val unexpectedName = TermName(c.freshName("unexpectedName"))
        val expectedNames = gropedDefs.map(_._1).mkString("'", "', '", "'")
        cq"""
            $unexpectedName =>
              $readerErrorCompanion.wrongJson("unexpected field '" + $unexpectedName + "', expected one of " + $expectedNames)($fieldNameTmp)
          """
      }
      else cq"_ => $tokenIteratorTerm.skipExpression()"
    }

    (res :+ defaultCase): List[CaseDef]
  }

  private def allocateRawFieldsPostProcessing(readerFields: List[ReaderField]): Tree = {
    type Res = (List[TermName], List[(Tree, String)], List[Tree])
    def buildTree(tpe: Type, jsonName: String, value: TermName, isInitialized: TermName): Res => Res = {
      case (processed, possiblyNotInitialized, trees) =>
        extractDefaultValue(tpe) match {
          case Some(defaultValue) =>
            val tree =
              q"""
             if(!$isInitialized) {
                $value = $defaultValue
                $isInitialized = true
             }
           """
            (value :: processed, possiblyNotInitialized, tree :: trees)

          case None =>
            (value :: processed, (q"!$isInitialized", jsonName) :: possiblyNotInitialized, trees)
        }
    }


    val (_, possiblyNotInitialized, defaultValues) = readerFields.foldLeft((List[TermName](), List[(Tree, String)](), List[Tree]())) {
      case (res, f: SimpleField) =>
        buildTree(f.tpe, f.jsonName, f.value, f.isInitialized)(res)

      case (tuple, f: ExtractedField) =>
        f.args.foldLeft(tuple) {
          case (res, FunctionArgument(Field.RawField(jsonName, tpe), value, isInitialized)) if !res._1.contains(value) =>
            buildTree(tpe, jsonName, value, isInitialized)(res)
          case (res, _) =>
            res
        }

      case (tuple, f: FromExtractedReader) =>
        f.args.foldLeft(tuple) {
          case (res, FunctionArgument(Field.RawField(jsonName, tpe), value, isInitialized)) if !res._1.contains(value) =>
            buildTree(tpe, jsonName, value, isInitialized)(res)
          case (res, _) =>
            res
        }

      case (res, _) =>
        res
    }

    possiblyNotInitialized.reverse match {
      case Nil =>
        q"..${defaultValues.reverse}"

      case xs@(headNotInit, _) :: tail =>
        val uninitializedFields = TermName(c.freshName("uninitializedFields"))
        val predicate = tail.foldLeft[Tree](q"$headNotInit")((a, b) => q"$a || ${b._1}")
        val fields = xs.map {
          case (notInit, name) =>
            q"""
               if($notInit) {
                 $uninitializedFields += $name
               }
             """
        }

        q"""
         ..${defaultValues.reverse}
         if($predicate) {
           val $uninitializedFields = new scala.collection.mutable.ArrayBuffer[String](${xs.size})
           ..$fields
           $readerErrorCompanion.wrongJson("Can not extract fields from json" + $uninitializedFields.mkString("'", "', '", "'"))
         }
       """
    }
  }

  private def allocateTransformationsLoop(readerFields: List[ReaderField]): Tree = {
    val (_, possiblyNotInitializedFields, defaults, loopActions) =  readerFields.foldLeft((List[TermName](), List[(Tree, String)](), List[Tree](), List[Tree]())) {
      case ((processed, possiblyNotInitialized, defaultTrees, loopTrees), field) =>
        def buildTransformation(name: String,
                                tpe: Type,
                                args: List[FunctionArgument],
                                value: TermName,
                                isInitialized: TermName,
                                tempIterator: Option[TermName])(valueAction: Tree) = {
          val start = tempIterator match {
            case Some(iter) =>
              q"!$isInitialized && $iter != null"
            case None =>
              q"!$isInitialized"
          }
          val canProcess = args.foldLeft[Tree](start) {
            case (current, FunctionArgument(_: Field.ClassField, _, argIsInitialized)) =>
              q"$current && $argIsInitialized"
            case (current, _) => //RawField already checked in allocateRawFieldsPostProcessing
              current
          }

          val loopAction =
            q"""
                  if($canProcess) {
                    $value = $valueAction
                    $isInitialized = true
                    $somethingChanged = true
                  }
               """

          extractDefaultValue(tpe) match {
            case Some(defaultValue) =>
              val tree =
                q"""
                     if(!$isInitialized) {
                        $value = $defaultValue
                        $isInitialized = true
                     }
                   """
              (value :: processed, possiblyNotInitialized, tree :: defaultTrees, loopAction :: loopTrees)

            case None =>
              (value :: processed, (q"!$isInitialized", name) :: possiblyNotInitialized, defaultTrees, loopAction :: loopTrees)
          }
        }

        field match {
          case ExtractedField(name, tpe, functionName, args, _, value, isInitialized) =>
            buildTransformation(name, tpe, args, value, isInitialized, None) {
              q"""
                 $functionName.apply(..${args.map(_.value)})
               """
            }

          case FromExtractedReader(name, tpe, _, functionName, args, _, value, isInitialized, tempIterator) =>
            buildTransformation(name, tpe, args, value, isInitialized, Some(tempIterator)) {
              q"""
                 implicit val $fieldNameTerm: $fieldNameType = $fieldNameTmp.appendFieldName($name)
                 $functionName.apply(..${args.map(_.value)}).read($tempIterator)
               """
            }

          case _ =>
            (processed, possiblyNotInitialized, defaultTrees, loopTrees)
        }
    }

    val loop =
      q"""
         var $somethingChanged = true
         while($somethingChanged) {
           $somethingChanged = false
           ..${loopActions.reverse}
         }
       """

    possiblyNotInitializedFields match {
      case Nil =>
        q"""
            ..$loop
            ..${defaults.reverse}
         """

      case xs@(headNotInit, _) :: tail =>
        val uninitializedFields = TermName(c.freshName("uninitializedFields"))
        val predicate = tail.foldLeft[Tree](q"$headNotInit")((a, b) => q"$a || ${b._1}")
        val fields = xs.map {
          case (notInit, name) =>
            q"""
               if($notInit) {
                 $uninitializedFields += $name
               }
             """
        }

        q"""
         ..$loop
         ..${defaults.reverse}
         if($predicate) {
           val $uninitializedFields = new scala.collection.mutable.ArrayBuffer[String](${xs.size})
           ..$fields
           $readerErrorCompanion.wrongJson("Can not extract fields" + $uninitializedFields.mkString("'", "', '", "'"))
         }
       """
    }
  }

  private def extractDefaultValue(tpe: Type): Option[Tree] = {
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

  private def provideThisReaderImplicit(tpe: Type): Tree = {
    c.typecheck(q"implicitly[$jsonReaderType[$tpe]]", silent = true) match {
      case EmptyTree =>
        val thisReaderTerm = TermName(c.freshName("thisReader"))
        q"implicit private[this] def $thisReaderTerm: $jsonReaderType[$tpe] = this"
      case _ => EmptyTree
    }
  }
}
