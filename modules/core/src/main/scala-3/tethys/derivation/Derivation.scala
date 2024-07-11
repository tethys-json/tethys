package tethys.derivation

import tethys.writers.tokens.TokenWriter
import tethys.readers.{FieldName, ReaderError}
import tethys.commons.{Token, TokenNode}
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.{JsonConfig, JsonObjectWriter, JsonReader, JsonWriter, ReaderBuilder, WriterBuilder}

import scala.Tuple2
import scala.annotation.tailrec
import scala.compiletime.{constValueTuple, summonInline}
import scala.quoted.*
import scala.collection.mutable
import scala.deriving.Mirror

private[tethys]
object Derivation:

  inline def deriveJsonWriterForProduct[T](inline config: WriterBuilder[T]): JsonObjectWriter[T] =
    ${ DerivationMacro.deriveJsonWriterForProduct[T]('{config})}

  inline def deriveJsonWriterForSum[T](inline config: JsonConfig[T]): JsonObjectWriter[T] =
    ${ DerivationMacro.deriveJsonWriterForSum[T]('{config}) }

  inline def deriveJsonReaderForProduct[T](inline config: ReaderBuilder[T]): JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForProduct[T]('{config})}

  inline def deriveJsonReaderForSum[T](inline config: JsonConfig[T]): JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForSum[T]('{config})}

object DerivationMacro:
  def deriveJsonWriterForProduct[T: Type](config: Expr[WriterBuilder[T]])(using quotes: Quotes): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes).deriveJsonWriterForProduct[T](config)

  def deriveJsonWriterForSum[T: Type](config: Expr[JsonConfig[T]])(using quotes: Quotes): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes).deriveJsonWriterForSum[T](config)

  def deriveJsonReaderForProduct[T: Type](config: Expr[ReaderBuilder[T]])(using quotes: Quotes): Expr[JsonReader[T]] =
    new DerivationMacro(quotes).deriveJsonReaderForProduct[T](config)

  def deriveJsonReaderForSum[T: Type](config: Expr[JsonConfig[T]])(using quotes: Quotes): Expr[JsonReader[T]] =
    new DerivationMacro(quotes).deriveJsonReaderForSum(config)

private[derivation]
class DerivationMacro(val quotes: Quotes) extends ConfigurationMacroUtils:
  import quotes.reflect.*

  def deriveJsonWriterForProduct[T: Type](config: Expr[WriterBuilder[T]]): Expr[JsonObjectWriter[T]] =
    val fields = prepareWriterProductFields(config)
    Block(
      deriveMissingWriters[T](fields.map(_.tpe)),
      '{
        new JsonObjectWriter[T]:
          override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
            ${
              Expr.block(
                fields.map { field => field.tpe.asType match
                  case '[f] =>
                    '{
                      searchJsonWriter[f]
                        .write(${ field.label }, ${ field.value('{ value }.asTerm).asExprOf[f] }, tokenWriter) }
                },
                '{}
              )
            }
      }.asTerm
    ).asExprOf[JsonObjectWriter[T]]

  def deriveJsonWriterForSum[T: Type](config: Expr[JsonConfig[T]]): Expr[JsonObjectWriter[T]] =
    val tpe = TypeRepr.of[T]
    val parsedConfig = parseSumConfig(config)
    val types = getAllChildren(tpe)
    Block(
      deriveMissingWritersForSum[T](types),
    '{
      new JsonObjectWriter[T]:
        override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
          $ {
            parsedConfig.discriminator.fold('{}) { case DiscriminatorConfig(label, tpe, discriminators) =>
              tpe.asType match
                case '[discriminatorType] =>
                  '{
                    searchJsonWriter[discriminatorType].write(
                      name = ${ Expr(label) },
                      value = ${ Select.unique('{ value }.asTerm, label).asExprOf[discriminatorType] },
                      tokenWriter = tokenWriter
                    )
                  }
            }
          }
          ${ matchByTypeAndWrite(
            term = '{ value }.asTerm,
            types = types,
            write = (ref, tpe) => tpe.asType match
              case '[t] =>
                '{ searchJsonObjectWriter[t].writeValues(${ref.asExprOf[t]}, tokenWriter) }
          )
          }
    }.asTerm
    ).asExprOf[JsonObjectWriter[T]]

  private def deriveMissingWritersForSum[T: Type](types: List[TypeRepr]): List[ValDef] =
    types.zipWithIndex
      .flatMap { case (tpe, idx) =>
        tpe.asType match
          case '[t] =>
            val symbol = Symbol.newVal(
              Symbol.spliceOwner,
              s"given_jsonWriter_$idx",
              TypeRepr.of[JsonObjectWriter[t]],
              Flags.Given,
              Symbol.noSymbol
            )
            Option.when(lookupOpt[JsonObjectWriter[t]].isEmpty)(
              ValDef(symbol, Some('{ JsonWriter.derived[t](using ${lookup[Mirror.Of[t]]}) }.asTerm))
            )
      }

  private def deriveMissingWriters[T: Type](tpes: List[TypeRepr]): List[ValDef] =
    tpes.distinct.zipWithIndex
      .flatMap { (tpe, idx) =>
        tpe.asType match
          case '[t] =>
            val symbol = Symbol.newVal(
                Symbol.spliceOwner,
                s"given_jsonWriter_$idx",
                TypeRepr.of[JsonWriter[t]],
                Flags.Given,
                Symbol.noSymbol
              )
            tpe match
              case or: OrType =>
                Option.when(lookupOpt[JsonWriter[t]].isEmpty)(
                  ValDef(
                    symbol,
                    Some(deriveOrTypeJsonWriter[t].asTerm)
                  )
                )
              case _ =>
                Option.when(lookupOpt[JsonWriter[t]].isEmpty)(
                  ValDef(
                    symbol,
                    Some('{JsonObjectWriter.derived[t](using ${lookup[Mirror.Of[t]]})}.asTerm)
                  )
                )
      }

  private def deriveOrTypeJsonWriter[T: Type]: Expr[JsonWriter[T]] =
    def collectTypes(tpe: TypeRepr, acc: List[TypeRepr] = Nil): List[TypeRepr] =
      tpe match
        case OrType(left, right) => collectTypes(left, Nil) ::: acc ::: collectTypes(right, Nil)
        case other => other :: acc

    val types = collectTypes(TypeRepr.of[T])
    val term = Block(
      deriveMissingWriters[T](types),
      '{
        new JsonWriter[T]:
          def write(value: T, tokenWriter: TokenWriter): Unit =
            ${
              matchByTypeAndWrite(
                term = '{ value }.asTerm,
                types = types,
                (ref, tpe) => tpe.asType match
                  case '[t] =>
                    '{ ${ lookup[JsonWriter[t]] }.write(${ref.asExprOf[t]}, tokenWriter) }
              )
            }
      }.asTerm
    )
    term.asExprOf[JsonWriter[T]]



  private def matchByTypeAndWrite(term: Term,
                                  types: List[TypeRepr],
                                  write: (Ref, TypeRepr) => Expr[Unit]): Expr[Unit] =
    Match(
      term,
      types.map { tpe =>
        tpe.asType match
          case '[t] =>
            val valDef = ValDef(
              Symbol.newVal(Symbol.spliceOwner, "value", tpe, Flags.EmptyFlags, Symbol.noSymbol),
              Some(Typed(term, TypeTree.of[t]))
            )
            CaseDef(
              pattern = Bind(valDef.symbol, Typed(Wildcard(), TypeTree.of[t])),
              guard = None,
              rhs = write(Ref(valDef.symbol), tpe).asTerm
            )
      }
    ).asExprOf[Unit]

  def deriveJsonReaderForProduct[T: Type](config: Expr[ReaderBuilder[T]]): Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val (fields, isStrict) = prepareReaderProductFields[T](config)
    val labelsToIndices = fields.map(field => field.name -> field.idx).toMap
    val existingLabels = fields.map(_.name).toSet
    val defaults: Map[String, Expr[Any]] = fields.flatMap(_.defaults(existingLabels)).distinctBy(_._1).toMap
    val requiredLabels = fields.flatMap(_.requiredLabels(existingLabels)).toSet -- defaults.keys
    val fieldsWithoutReader = fields.collect { case (field: ReaderField.Extracted) if field.reader => field.name }
    val fieldNamesWithTypes = fields.flatMap(_.readerTypes(existingLabels)).distinctBy(_._1)

    val sortedFields: List[(String, Int, Option[Expr[Any => Any]], List[(String, Option[Int])], Boolean)] =
      fields.map {
        case field: ReaderField.Basic =>
          (field.name, field.idx, field.extractor.map(_._2), Nil, false)
        case field: ReaderField.Extracted =>
          (field.name, field.idx, Some(field.lambda), field.extractors.map((name, _) => (name -> labelsToIndices.get(name))), field.reader)
      }

   Block(
      deriveMissingReaders(fieldNamesWithTypes.map(_._2)),
      '{
        new JsonReader[T]:
          given JsonReader[T] = this

          def read(it: TokenIterator)(implicit fieldName: FieldName) =
            if !it.currentToken().isObjectStart then
              ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)

            it.nextToken()
            val collectedValues: mutable.Map[String, Any] = ${
              if defaults.isEmpty then
                '{ mutable.Map.empty }
              else
                '{
                  val fallback = ${ defaults.exprOfMap[String, Any] }
                  mutable.Map.empty.withDefault(fallback)
                }
            }

            val missingFields: mutable.Set[String] = ${ requiredLabels.exprOfMutableSet }
            val resultFields = mutable.Map.empty[Int, Any]
            lazy val fieldsForExtractedReader = mutable.Map.empty[String, TokenIterator]
            lazy val fieldsWithoutReaders: mutable.Set[String] = ${ fieldsWithoutReader.exprOfMutableSet }
            while (!it.currentToken().isObjectEnd)
              val jsonName = it.fieldName()
              it.nextToken()
              val currentIt = it.collectExpression()

              ${
                Match(
                  '{jsonName}.asTerm,
                  fieldNamesWithTypes.map { (name, tpe) =>
                    tpe.asType match {
                      case '[t] =>
                        CaseDef(
                          Literal(StringConstant(name)),
                          None,
                          '{
                            collectedValues += ${Expr(name)} -> summonInline[JsonReader[t]]
                              .read(currentIt.copy())(fieldName.appendFieldName(${Expr(name)}))
                            missingFields -= ${Expr(name)}
                            ()
                          }.asTerm
                        )
                    }
                  } :+ {
                    def ifFoundFieldForReader: Expr[Unit] = '{
                      fieldsForExtractedReader.update(jsonName, currentIt)
                      missingFields -= jsonName
                      fieldsWithoutReaders -= jsonName
                      ()
                    }

                    def ifUnknownFieldWhileStrict: Expr[Unit] = '{
                      val expectedNames = (collectedValues.keySet ++ missingFields ++ ${ defaults.keySet.exprOfSet }).mkString("'", "', '", "'")
                      ReaderError.wrongJson(s"unexpected field '$jsonName', expected one of $expectedNames")
                      ()
                    }
                    CaseDef(
                      Wildcard(),
                      None,
                      if fieldsWithoutReader.nonEmpty && isStrict then
                        '{
                          if fieldsWithoutReaders.contains(jsonName) then ${ifFoundFieldForReader}
                          else if ${ Expr(isStrict) } then ${ifUnknownFieldWhileStrict}
                        }.asTerm
                      else if fieldsWithoutReader.nonEmpty && !isStrict then
                        '{ if fieldsWithoutReaders.contains(jsonName) then ${ ifFoundFieldForReader } else ()}.asTerm
                      else if fieldsWithoutReader.isEmpty && isStrict then
                        ifUnknownFieldWhileStrict.asTerm
                      else '{}.asTerm
                    )
                  }
                ).asExprOf[Unit]
              }

            it.nextToken()

            if (missingFields.nonEmpty)
              ReaderError.wrongJson("Can not extract fields from json: " + missingFields.mkString(", "))

            ${
              Expr.block(
                sortedFields.map { (name, idx, function, dependencies, extractReader) =>
                  dependencies match
                    case Nil =>
                      '{ resultFields += Tuple2(${Expr(idx)}, ${
                        function match
                          case Some(buildField) =>
                            '{ ${buildField}.asInstanceOf[Any => Any].apply(collectedValues(${ Expr(name) })) }
                          case None =>
                            '{ collectedValues(${ Expr(name) }) }
                      }) }

                    case dependencies =>
                      def fieldsToBuildFrom(depName: String, depIdx: Option[Int]): Expr[Any] =
                        depIdx.fold('{ collectedValues(${ Expr(depName) }) })(idx => '{ resultFields(${ Expr(idx) }) })

                      val value = (function, dependencies) match
                        case (None, _) =>
                          report.errorAndAbort("Internal error, function not provided for dependency")

                        case (Some(buildField), List((depName, depIdxOpt))) =>
                          '{ ${ buildField }.apply(${fieldsToBuildFrom(depName, depIdxOpt)}) }

                        case (Some(buildField), dependencies) =>
                          '{ ${ buildField }.apply(Tuple.fromArray(Array(${ Varargs(dependencies.map(fieldsToBuildFrom)) }: _*))) }

                      if (!extractReader)
                        '{ resultFields += ${Expr(idx)} -> ${ value } }
                      else
                        '{
                          val read: TokenIterator => Any = ${value}.asInstanceOf[JsonReader[Any]].read(_)(fieldName.appendFieldName(${Expr(name)}))
                          fieldsForExtractedReader.get(${Expr(name)}) match
                            case Some(iterator) =>
                              resultFields += ${Expr(idx)} -> read(iterator)
                            case _ =>
                              try
                                resultFields += ${Expr(idx)} -> read(QueueIterator(List(TokenNode.NullValueNode)))
                                fieldsWithoutReaders -= ${Expr(name)}
                              catch
                                case _: ReaderError =>
                        }
                },
                '{}
              )
            }

            ${ if (fieldsWithoutReader.nonEmpty)
              '{
                if (fieldsWithoutReaders.nonEmpty)
                  ReaderError.wrongJson("Can not extract fields from json: " + fieldsWithoutReaders.mkString(", "))
              }
              else '{}
            }

            summonInline[scala.deriving.Mirror.ProductOf[T]].fromProduct:
              new Product:
                def productArity = resultFields.size
                def productElement(n: Int) = resultFields(n)
                def canEqual(that: Any) = that match
                  case that: Product if that.productArity == productArity => true
                  case _ => false
      }.asTerm
    ).asExprOf[JsonReader[T]]


  def deriveJsonReaderForSum[T: Type](config: Expr[JsonConfig[T]]): Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val parsed = parseSumConfig(config)
    val children = getAllChildren(tpe)
    parsed.discriminator match
      case Some(DiscriminatorConfig(label, tpe, discriminators)) => tpe.asType match
        case '[discriminator]  =>
          val (discriminatorStats, discriminatorRefs) = discriminators.zipWithIndex.map((term, idx) =>
            val stat = ValDef(
              Symbol.newVal(Symbol.spliceOwner, s"Discriminator_$idx", term.tpe, Flags.Private, Symbol.noSymbol),
              Some(term)
            )
            (stat, Ref(stat.symbol))
          ).unzip
          Block(
            deriveMissingReaders(children) ++ discriminatorStats,
            '{
              JsonReader.builder
                .addField[discriminator](
                  name = ${Expr(label)},
                  jsonReader = searchJsonReader[discriminator]
                )
                .selectReader[T] { discriminator =>
                  ${
                    Match(
                      '{discriminator}.asTerm,
                      children.zip(discriminatorRefs).map((tpe, branchDiscriminator) => tpe.asType match
                        case '[t] =>
                          CaseDef(
                            branchDiscriminator,
                            None,
                            Typed('{searchJsonReader[t]}.asTerm, TypeTree.of[JsonReader[? <: T]])
                          )
                    ) :+ CaseDef(Wildcard(), None,
                        '{ReaderError.wrongJson(s"Unexpected discriminator found: $discriminator")(using FieldName(${Expr(label)})) }.asTerm
                      )
                    ).asExprOf[JsonReader[? <: T]]
                  }
                }
            }.asTerm
          ).asExprOf[JsonReader[T]]

      case None =>
        report.errorAndAbort("Discriminator is required to derive JsonReader for sum type. Use JsonConfig[T].discriminateBy(_.field)")


  private def deriveMissingReaders(tpes: List[TypeRepr]): List[ValDef] =
    tpes.distinct.zipWithIndex.flatMap { (tpe, idx) =>
      tpe.asType match
        case '[t] =>
          lookupOpt[JsonReader[t]] match
            case Some(reader) =>
              None
            case None =>
              Some(
                ValDef(
                  Symbol.newVal(
                    Symbol.spliceOwner,
                    s"given_jsonReader_$idx",
                    TypeRepr.of[JsonReader[t]],
                    Flags.Given,
                    Symbol.noSymbol
                  ),
                  Some('{ JsonReader.derived[t](using ${ lookup[scala.deriving.Mirror.Of[t]] }) }.asTerm)
                )
              )
    }







