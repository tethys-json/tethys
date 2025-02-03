package tethys.derivation

import tethys.derivation.builder.{
  ReaderDerivationConfig,
  WriterDerivationConfig
}
import tethys.writers.tokens.TokenWriter
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{
  JsonObjectWriter,
  JsonReader,
  JsonWriter,
  ReaderBuilder,
  WriterBuilder,
  JsonConfiguration
}
import scala.Tuple2
import scala.annotation.tailrec
import scala.compiletime.{constValueTuple, summonInline}
import scala.quoted.*
import scala.collection.mutable
import scala.deriving.Mirror

private[tethys] object Derivation:

  inline def deriveJsonWriterForProduct[T](
      inline config: WriterBuilder[T],
      inline jsonConfig: JsonConfiguration
  ): JsonObjectWriter[T] =
    ${
      DerivationMacro
        .deriveJsonWriterForProduct[T]('{ config }, '{ jsonConfig })
    }

  inline def deriveJsonWriterForSum[T]: JsonObjectWriter[T] =
    ${ DerivationMacro.deriveJsonWriterForSum[T] }

  inline def deriveJsonReaderForProduct[T](
      inline config: ReaderBuilder[T],
      inline jsonConfig: JsonConfiguration
  ): JsonReader[T] =
    ${
      DerivationMacro
        .deriveJsonReaderForProduct[T]('{ config }, '{ jsonConfig })
    }

  @deprecated
  inline def deriveJsonReaderForProductLegacy[T](
      inline config: ReaderDerivationConfig
  )(using mirror: Mirror.ProductOf[T]): JsonReader[T] =
    ${
      DerivationMacro
        .deriveJsonReaderForProductLegacy[T]('{ config }, '{ mirror })
    }

  @deprecated
  inline def deriveJsonWriterForProductLegacy[T](
      inline config: WriterDerivationConfig
  )(using mirror: Mirror.ProductOf[T]): JsonObjectWriter[T] =
    ${
      DerivationMacro
        .deriveJsonWriterForProductLegacy[T]('{ config }, '{ mirror })
    }

  @deprecated
  inline def deriveJsonWriterForSumLegacy[T](
      inline config: WriterDerivationConfig
  ): JsonObjectWriter[T] =
    ${ DerivationMacro.deriveJsonWriterForSumLegacy[T]('{ config }) }

  inline def deriveJsonReaderForSum[T]: JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForSum[T] }

object DerivationMacro:
  def deriveJsonWriterForProduct[T: Type](
      config: Expr[WriterBuilder[T]],
      jsonConfig: Expr[JsonConfiguration]
  )(using
      quotes: Quotes
  ): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes)
      .deriveJsonWriterForProduct[T](config, jsonConfig)

  def deriveJsonWriterForSum[T: Type](using
      quotes: Quotes
  ): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes).deriveJsonWriterForSum[T](None)

  def deriveJsonReaderForProduct[T: Type](
      config: Expr[ReaderBuilder[T]],
      jsonConfig: Expr[JsonConfiguration]
  )(using
      quotes: Quotes
  ): Expr[JsonReader[T]] =
    new DerivationMacro(quotes)
      .deriveJsonReaderForProduct[T](config, jsonConfig)

  def deriveJsonReaderForSum[T: Type](using
      quotes: Quotes
  ): Expr[JsonReader[T]] =
    new DerivationMacro(quotes).deriveJsonReaderForSum[T]

  @deprecated
  def deriveJsonReaderForProductLegacy[T: Type](
      config: Expr[ReaderDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  )(using quotes: Quotes): Expr[JsonReader[T]] =
    new DerivationMacro(quotes)
      .deriveJsonReaderForProductLegacy[T](config, mirror)

  @deprecated
  def deriveJsonWriterForProductLegacy[T: Type](
      config: Expr[WriterDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  )(using quotes: Quotes): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes)
      .deriveJsonWriterForProductLegacy[T](config, mirror)

  @deprecated
  def deriveJsonWriterForSumLegacy[T: Type](
      config: Expr[WriterDerivationConfig]
  )(using quotes: Quotes): Expr[JsonObjectWriter[T]] =
    new DerivationMacro(quotes).deriveJsonWriterForSumLegacy[T](config)

private[derivation] class DerivationMacro(val quotes: Quotes)
    extends ConfigurationMacroUtils:
  import quotes.reflect.*

  def deriveJsonWriterForProduct[T: Type](
      config: Expr[WriterBuilder[T]],
      jsonConfig: Expr[JsonConfiguration]
  ): Expr[JsonObjectWriter[T]] =
    val fields = prepareWriterProductFields(config, jsonConfig)
    val (missingWriters, refs) =
      deriveMissingWriters(TypeRepr.of[T], fields.map(_.tpe))
    val writer = Block(
      missingWriters,
      '{
        new JsonObjectWriter[T]:
          override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
            ${
              Expr.block(
                fields.map { field =>
                  field.tpe.asType match
                    case '[f] =>
                      val writer = refs
                        .get(field.tpe)
                        .fold(lookup[JsonWriter[f]])(_.asExprOf[JsonWriter[f]])
                      '{
                        $writer.write(
                          ${ field.label },
                          ${ field.value('{ value }.asTerm).asExprOf[f] },
                          tokenWriter
                        )
                      }
                },
                '{}
              )
            }
      }.asTerm
    )
    writer.asExprOf[JsonObjectWriter[T]]

  def deriveJsonWriterForSum[T: Type](
      legacyConfig: Option[DiscriminatorConfig]
  ): Expr[JsonObjectWriter[T]] =
    val tpe = TypeRepr.of[T]
    val parsedConfig = parseSumConfig[T]
    val types = getAllChildren(tpe)
    val (missingWriters, refs) = deriveMissingWritersForSum(types)
    val mirror = '{ summonInline[Mirror.SumOf[T]] }
    val writer = Block(
      missingWriters,
      '{
        new JsonObjectWriter[T]:
          override def writeValues(value: T, tokenWriter: TokenWriter): Unit =
            ${
              legacyConfig.fold('{}) {
                case DiscriminatorConfig(label, tpe, values) =>
                  '{
                    JsonWriter.stringWriter.write(
                      name = ${ Expr(label) },
                      value = ${
                        Expr.ofList(
                          types.map(t =>
                            Expr(t.typeSymbol.name.filterNot(_ == '$'))
                          )
                        )
                      }.apply(${ mirror }.ordinal(value)),
                      tokenWriter = tokenWriter
                    )
                  }
              }
            }
            ${
              parsedConfig.discriminator.fold('{}) {
                case DiscriminatorConfig(label, tpe, discriminators) =>
                  tpe.asType match
                    case '[discriminatorType] =>
                      '{
                        ${ lookup[JsonWriter[discriminatorType]] }.write(
                          name = ${ Expr(label) },
                          value = ${
                            Select
                              .unique('{ value }.asTerm, label)
                              .asExprOf[discriminatorType]
                          },
                          tokenWriter = tokenWriter
                        )
                      }
              }
            }
            ${
              matchByTypeAndWrite(
                term = '{ value }.asTerm,
                types = types,
                write = (ref, tpe) =>
                  tpe.asType match
                    case '[t] =>
                      val writer = refs
                        .get(tpe)
                        .fold(lookup[JsonObjectWriter[t]])(
                          _.asExprOf[JsonObjectWriter[t]]
                        )
                      '{
                        ${ writer }
                          .writeValues(${ ref.asExprOf[t] }, tokenWriter)
                      }
              )
            }
      }.asTerm
    )
    writer.asExprOf[JsonObjectWriter[T]]

  private def deriveMissingWritersForSum(
      types: List[TypeRepr]
  ): (List[ValDef], Map[TypeRepr, Ref]) =
    val (stats, refs) = types.flatMap { tpe =>
      tpe.asType match
        case '[t] =>
          val symbol = Symbol.newVal(
            Symbol.spliceOwner,
            s"given_JsonWriter_${tpe.show(using Printer.TypeReprShortCode)}",
            TypeRepr.of[JsonObjectWriter[t]],
            Flags.Given,
            Symbol.noSymbol
          )
          val valDef = Option.when(lookupOpt[JsonObjectWriter[t]].isEmpty)(
            ValDef(
              symbol,
              Some('{
                JsonObjectWriter.derived[t](using ${ lookup[Mirror.Of[t]] })
              }.asTerm)
            )
          )
          valDef.map(valDef => (valDef, (tpe, Ref(valDef.symbol))))
    }.unzip
    (stats, refs.toMap)

  private def tpeAsString(tpe: TypeRepr) =
    tpe.dealias.show(using Printer.TypeReprCode)

  private def deriveMissingWriters(
      thisTpe: TypeRepr,
      tpes: List[TypeRepr]
  ): (List[ValDef], Map[TypeRepr, Ref]) =
    val (stats, refs) = distinct(tpes)
      .filterNot(isRecursive(thisTpe, _))
      .flatMap { tpe =>
        tpe.asType match
          case '[t] =>
            lookupOpt[JsonWriter[t]].map {
              _.asTerm match
                case ident: Ident =>
                  Left(ident)
                case other =>
                  Right(other)
            } match
              case Some(Left(writer)) =>
                None

              case other =>
                val valDef = ValDef(
                  Symbol.newVal(
                    Symbol.spliceOwner,
                    s"given_JsonWriter_${tpe.show(using Printer.TypeReprShortCode)}",
                    TypeRepr.of[JsonWriter[t]],
                    Flags.Given,
                    Symbol.noSymbol
                  ),
                  Some(
                    other
                      .flatMap(_.toOption)
                      .getOrElse {
                        tpe match
                          case or: OrType =>
                            deriveOrTypeJsonWriter[t].asTerm
                          case _ =>
                            '{
                              JsonObjectWriter.derived[t](using
                                ${ lookup[scala.deriving.Mirror.Of[t]] }
                              )
                            }.asTerm
                      }
                  )
                )
                Some((valDef, (tpe, Ref(valDef.symbol))))
      }
      .unzip
    (stats, refs.toMap)

  private def deriveOrTypeJsonWriter[T: Type]: Expr[JsonWriter[T]] =
    def collectTypes(tpe: TypeRepr, acc: List[TypeRepr] = Nil): List[TypeRepr] =
      tpe match
        case OrType(left, right) =>
          collectTypes(left, Nil) ::: acc ::: collectTypes(right, Nil)
        case other => other :: acc

    val types = collectTypes(TypeRepr.of[T])
    val (missingWriters, refs) = deriveMissingWriters(TypeRepr.of[T], types)
    val term = Block(
      missingWriters,
      '{
        new JsonWriter[T]:
          def write(value: T, tokenWriter: TokenWriter): Unit =
            ${
              matchByTypeAndWrite(
                term = '{ value }.asTerm,
                types = types,
                (ref, tpe) =>
                  tpe.asType match
                    case '[t] =>
                      val writer = refs
                        .get(tpe)
                        .fold(lookup[JsonWriter[t]])(_.asExprOf[JsonWriter[t]])
                      '{ ${ writer }.write(${ ref.asExprOf[t] }, tokenWriter) }
              )
            }
      }.asTerm
    )
    term.asExprOf[JsonWriter[T]]

  private def matchByTypeAndWrite(
      term: Term,
      types: List[TypeRepr],
      write: (Ref, TypeRepr) => Expr[Unit]
  ): Expr[Unit] =
    Match(
      term,
      types.map { tpe =>
        tpe.asType match
          case '[t] =>
            val valDef = ValDef(
              Symbol.newVal(
                Symbol.spliceOwner,
                "value",
                tpe,
                Flags.EmptyFlags,
                Symbol.noSymbol
              ),
              Some(Typed(term, TypeTree.of[t]))
            )
            CaseDef(
              pattern = Bind(valDef.symbol, Typed(Wildcard(), TypeTree.of[t])),
              guard = None,
              rhs = write(Ref(valDef.symbol), tpe).asTerm
            )
      }
    ).asExprOf[Unit]

  def deriveJsonReaderForProduct[T: Type](
      config: Expr[ReaderBuilder[T]],
      jsonConfig: Expr[JsonConfiguration]
  ): Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val (fields, isStrict) = prepareReaderProductFields[T](config, jsonConfig)
    val existingLabels = fields.map(_.name).toSet
    val fieldsWithoutReader = fields.collect {
      case field: ReaderField.Extracted if field.reader => field.name
    }

    val (basicFields, extractedFields) = fields.partitionMap {
      case field: ReaderField.Basic     => Left(field)
      case field: ReaderField.Extracted => Right(field)
    }

    val expectedFieldNames =
      basicFields.map(_.name).toSet ++ extractedFields.flatMap(
        _.extractors.map(_._1)
      ) -- extractedFields.map(_.name)

    def failIfNotInitialized(fieldName: Expr[FieldName]): Expr[Unit] =
      basicFields.filterNot(_.default.nonEmpty) match
        case refs @ head :: tail =>
          val boolExpr = tail.foldLeft('{
            !${ head.initRef.asExprOf[Boolean] }
          }) { (acc, el) =>
            '{ ${ acc } || !${ el.initRef.asExprOf[Boolean] } }
          }
          '{
            if { $boolExpr } then
              val uninitializedFields =
                new scala.collection.mutable.ArrayBuffer[String](${
                  Expr(refs.size)
                })
              ${
                Expr.block(
                  refs.map { ref =>
                    '{
                      if !${ ref.initRef.asExprOf[Boolean] } then
                        uninitializedFields += ${ Expr(ref.name) }
                    }
                  },
                  '{}
                )
              }
              ReaderError.wrongJson(
                "Can not extract fields from json: " + uninitializedFields
                  .mkString("'", "', '", "'")
              )(${ fieldName })
          }

        case Nil =>
          '{}

    if tpe.typeSymbol.flags.is(Flags.Module) then
      '{ JsonReader.const(${ Ref(tpe.termSymbol).asExprOf[T] }) }
    else
      val (missingReaders, refs) =
        deriveMissingReaders(tpe, basicFields.map(_.tpe))
      val term = Block(
        missingReaders,
        '{
          new JsonReader[T]:
            given JsonReader[T] = this
            override def read(it: TokenIterator)(using fieldName: FieldName) =
              if !it.currentToken().isObjectStart then
                ReaderError.wrongJson(
                  "Expected object start but found: " + it
                    .currentToken()
                    .toString
                )
              else
                it.nextToken()
                ${
                  Block(
                    fields.flatMap(_.initialize),
                    '{
                      while (!it.currentToken().isObjectEnd)
                        val jsonName = it.fieldName()
                        it.nextToken()
                        ${
                          Match(
                            selector = '{ jsonName }.asTerm,
                            cases = fields.flatMap(
                              _.initializeFieldCase(
                                refs,
                                '{ it },
                                '{ fieldName }
                              )
                            ) :+
                              CaseDef(
                                Wildcard(),
                                None,
                                if isStrict then
                                  '{
                                    ReaderError.wrongJson(
                                      s"unexpected field '$jsonName', expected one of ${${ Expr(expectedFieldNames.mkString("'", "', '", "'")) }}"
                                    )
                                  }.asTerm
                                else '{ it.skipExpression(); () }.asTerm
                              )
                          ).asExprOf[Unit]
                        }
                      it.nextToken()

                      ${ failIfNotInitialized('{ fieldName }) }

                      ${
                        val allRefs =
                          fields.map(field => field.name -> field.ref).toMap
                        Expr.block(
                          extractedFields
                            .flatMap(_.extract(allRefs, '{ fieldName }))
                            .map(_.asExprOf[Unit]),
                          '{}
                        )
                      }

                      ${
                        New(TypeTree.of[T])
                          .select(tpe.classSymbol.get.primaryConstructor)
                          .appliedToTypes(tpe.typeArgs)
                          .appliedToArgs(
                            fields
                              .filterNot(_.idx == -1)
                              .sortBy(_.idx)
                              .map(_.ref)
                          )
                          .asExprOf[T]
                      }

                    }.asTerm
                  ).asExprOf[T]
                }
        }.asTerm
      )
      term.asExprOf[JsonReader[T]]

  def deriveJsonReaderForSum[T: Type]: Expr[JsonReader[T]] =
    val tpe = TypeRepr.of[T]
    val parsed = parseSumConfig[T]
    val children = getAllChildren(tpe)
    parsed.discriminator match
      case Some(DiscriminatorConfig(label, tpe, discriminators)) =>
        tpe.asType match
          case '[discriminator] =>
            val (discriminatorStats, discriminatorRefs) =
              discriminators.zipWithIndex
                .map((term, idx) =>
                  val stat = ValDef(
                    Symbol.newVal(
                      Symbol.spliceOwner,
                      s"Discriminator_$idx",
                      term.tpe,
                      Flags.Private,
                      Symbol.noSymbol
                    ),
                    Some(term)
                  )
                  (stat, Ref(stat.symbol))
                )
                .unzip
            val (readers, refs) = deriveMissingReaders(TypeRepr.of[T], children)
            val term = Block(
              readers ++ discriminatorStats,
              '{
                JsonReader.builder
                  .addField[discriminator](
                    name = ${ Expr(label) },
                    jsonReader = ${ lookup[JsonReader[discriminator]] }
                  )
                  .selectReader[T] { discriminator =>
                    ${
                      Match(
                        '{ discriminator }.asTerm,
                        children
                          .zip(discriminatorRefs)
                          .map((tpe, branchDiscriminator) =>
                            tpe.asType match
                              case '[t] =>
                                CaseDef(
                                  branchDiscriminator,
                                  None,
                                  Typed(
                                    refs.getOrElse(
                                      tpe,
                                      lookup[JsonReader[t]].asTerm
                                    ),
                                    TypeTree.of[JsonReader[? <: T]]
                                  )
                                )
                          ) :+ CaseDef(
                          Wildcard(),
                          None,
                          '{
                            ReaderError.wrongJson(
                              s"Unexpected discriminator found: $discriminator"
                            )(using FieldName(${ Expr(label) }))
                          }.asTerm
                        )
                      ).asExprOf[JsonReader[? <: T]]
                    }
                  }
              }.asTerm
            )
            term.asExprOf[JsonReader[T]]

      case None =>
        report.errorAndAbort(
          "Discriminator is required to derive JsonReader for sum type. Use @selector annotation"
        )

  private def distinct(tpes: List[TypeRepr]) =
    tpes.foldLeft(List.empty[TypeRepr]) { (acc, tpe) =>
      if (acc.exists(_ =:= tpe)) acc
      else tpe :: acc
    }

  private def isRecursive(tpe: TypeRepr, childTpe: TypeRepr): Boolean =
    tpe =:= childTpe || (childTpe match
      case AppliedType(_, types) => types.exists(isRecursive(tpe, _))
      case _                     => false
    )

  private def deriveMissingReaders(
      thisTpe: TypeRepr,
      tpes: List[TypeRepr]
  ): (List[ValDef], Map[TypeRepr, Ref]) =
    val (stats, refs) = distinct(tpes)
      .filterNot(isRecursive(thisTpe, _))
      .flatMap { tpe =>
        tpe.asType match
          case '[t] =>
            lookupOpt[JsonReader[t]].map {
              _.asTerm match
                case ident: Ident =>
                  Left(ident)
                case other =>
                  Right(other)
            } match
              case Some(Left(_)) =>
                None

              case other =>
                val valDef = ValDef(
                  Symbol.newVal(
                    Symbol.spliceOwner,
                    s"given_JsonReader_${tpe.show(using Printer.TypeReprShortCode)}",
                    TypeRepr.of[JsonReader[t]],
                    Flags.Given,
                    Symbol.noSymbol
                  ),
                  Some(
                    other
                      .flatMap(_.toOption)
                      .getOrElse {
                        '{
                          JsonReader.derived[t](using
                            ${ lookup[scala.deriving.Mirror.Of[t]] }
                          )
                        }.asTerm
                      }
                  )
                )
                Some((valDef, (tpe, Ref(valDef.symbol))))
      }
      .unzip
    (stats, refs.toMap)

  @deprecated
  def deriveJsonReaderForProductLegacy[T: Type](
      config: Expr[ReaderDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  ): Expr[JsonReader[T]] =
    deriveJsonReaderForProduct(
      parseLegacyReaderDerivationConfig(config, mirror),
      '{ JsonConfiguration.default }
    )

  @deprecated
  def deriveJsonWriterForProductLegacy[T: Type](
      config: Expr[WriterDerivationConfig],
      mirror: Expr[Mirror.ProductOf[T]]
  ): Expr[JsonObjectWriter[T]] =
    deriveJsonWriterForProduct(
      parseLegacyWriterDerivationConfig(config, mirror),
      '{ JsonConfiguration.default }
    )

  @deprecated
  def deriveJsonWriterForSumLegacy[T: Type](
      config: Expr[WriterDerivationConfig]
  ): Expr[JsonObjectWriter[T]] =
    deriveJsonWriterForSum(Some(parseLegacyDiscriminator(config)))
