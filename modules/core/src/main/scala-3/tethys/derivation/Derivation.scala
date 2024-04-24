package tethys.derivation

import tethys.commons.TokenNode
import tethys.writers.tokens.TokenWriter
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.readers.FieldName
import tethys.readers.ReaderError
import tethys.{JsonConfig, JsonFieldStyle, JsonObjectWriter, JsonReader, JsonWriter}

import scala.compiletime.{constValueTuple, summonInline}
import scala.deriving.Mirror
import scala.collection.mutable

private[tethys]
object Derivation:

  inline def show[T](inline value: T): Unit =
    ${ DerivationMacro.show('{ value }) }

  inline def showTree[T](inline value: T): Unit =
    ${ DerivationMacro.showTree[T]('{ value }) }

  inline def fieldJsonWriter[T, F]: JsonWriter[F] =
    ${ DerivationMacro.fieldJsonWriter[T, F] }

  inline def parseJsonWriterProductConfig[T](inline config: JsonWriter.ProductConfig[T]): JsonWriterProductConfigParsed[T] =
    ${ DerivationMacro.parseJsonWriterProductConfig[T]('{config}) }

  inline def parseJsonReaderProductConfig[T](inline config: JsonReader.ProductConfig[T]): JsonReaderProductConfigParsed =
    ${ DerivationMacro.parseJsonReaderProductConfig[T]('{config})}

  inline def writeDiscriminator[T](inline config: JsonConfig[T]): (T, TokenWriter) => Unit =
    ${ DerivationMacro.writeDiscriminator[T]('{config})}

  inline def deriveJsonReaderForSum[T](inline config: JsonConfig[T], inline readers: List[JsonReader[?]]): JsonReader[T] =
    ${ DerivationMacro.deriveJsonReaderForSum[T]('{config}, '{readers})}



private[derivation]
object DerivationMacro:
  import scala.quoted.*

  def show[T: Type](value: Expr[T])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{ println(${Expr(value.asTerm.show(using Printer.TreeShortCode))}) }


  def showTree[T: Type](value: Expr[T])(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    '{ println(${ Expr(value.asTerm.show(using Printer.TreeStructure)) }) }

  def parseJsonWriterProductConfig[T: Type](config: Expr[JsonWriter.ProductConfig[T]])(using quotes: Quotes): Expr[JsonWriterProductConfigParsed[T]] =
    val utils = new ConfigurationMacroUtils
    import utils.quotes.reflect.*
    '{
      JsonWriterProductConfigParsed(
        ${
          Expr.ofList(
            utils.prepareWriterProductFields[T](config).map { field =>
              field.tpe.asType match
                case '[fieldType] => '{
                  JsonWriterProductConfigParsed.Field(
                    label = ${ field.label },
                    function = (value: T) => ${ field.value('{ value }.asTerm).asExprOf[fieldType] },
                    writer = Derivation.fieldJsonWriter[T, fieldType]
                  )
                }
            }
          )
        }
      )
    }

  def fieldJsonWriter[T: Type, F: Type](using quotes: Quotes): Expr[JsonWriter[F]] =
    val utils = new ConfigurationMacroUtils
    import utils.quotes.reflect.*
    val tpe = TypeRepr.of[F]

    def allocateWriters(types: List[TypeRepr]): (List[Statement], List[Ref]) = types.zipWithIndex.map { (tpe, idx) =>
      tpe.asType match
        case '[fieldType] =>
          val term = ValDef(
            Symbol.newVal(Symbol.spliceOwner, s"jsonWriter$idx", TypeRepr.of[JsonWriter[fieldType]], Flags.Lazy, Symbol.noSymbol),
            Some('{ JsonObjectWriterDerivation.summonOrDeriveJsonWriterForField[T, fieldType] }.asTerm)
          )
          (term, Ref(term.symbol))
    }.unzip

    def loop(tpe: TypeRepr, acc: List[TypeRepr] = Nil): List[TypeRepr] =
      tpe match
        case OrType(left, right) => loop(left, Nil) ::: acc ::: loop(right, Nil)
        case other => other :: acc

    val types = loop(tpe)

    if types.length <= 1 then
      '{ JsonObjectWriterDerivation.summonOrDeriveJsonWriterForField[T, F] }
    else
      val (writers, writersRefs) = allocateWriters(types)

      Block(
        writers,
        '{
          new JsonWriter[F]:
            def write(value: F, tokenWriter: TokenWriter): Unit =
              ${
                val term ='{ value }.asTerm
                Match(
                  term ,
                  types.zip(writersRefs).map { (tpe, writer) =>
                    tpe.asType match
                      case '[t] =>
                        val valDef = ValDef(
                          Symbol.newVal(Symbol.spliceOwner, "value", tpe, Flags.EmptyFlags, Symbol.noSymbol),
                          Some(Typed(term, TypeTree.of[t]))
                        )
                        val ref = Ref(valDef.symbol)

                        CaseDef(
                          pattern = Bind(valDef.symbol, Typed(Wildcard(), TypeTree.of[t])),
                          guard = None,
                          '{ ${ writer.asExprOf[JsonWriter[t]] }.write(${ ref.asExprOf[t] }, tokenWriter) }.asTerm
                        )

                  }
                ).asExprOf[Unit]
              }
        }.asTerm
      ).asExprOf[JsonWriter[F]]


  def parseJsonReaderProductConfig[T: Type](config: Expr[JsonReader.ProductConfig[T]])(using quotes: Quotes): Expr[JsonReaderProductConfigParsed] =
    val utils = new ConfigurationMacroUtils
    import utils.quotes.reflect.*
    import utils.{exprOfMap, exprOfSet}
    val tpe = TypeRepr.of[T]
    val (fields, isStrict) = utils.prepareReaderProductFields[T](config)
    val labelsToIndices = fields.map(field => field.name -> field.idx).toMap
    val existingLabels = fields.map(_.name).toSet
    val defaults: Map[String, Expr[Any]] = fields.flatMap(_.defaults(existingLabels)).distinctBy(_._1).toMap
    val requiredLabels: Set[String] = fields.flatMap(_.requiredLabels(existingLabels)).toSet -- defaults.keys

    val fieldsWithoutReader = fields.collect { case (field: utils.ReaderField.Extracted) if field.reader => field.name }

    val readers = fields.flatMap(_.readerTypes(existingLabels)).distinctBy(_._1).map { (name, tpe) =>
      tpe.asType match {
        case '[fieldType] => name -> '{ JsonReaderDerivation.summonOrDeriveJsonReaderForField[T, fieldType] }
      }
    }.exprOfMap[String, JsonReader[?]]

    val sortedFields: Expr[List[JsonReaderProductConfigParsed.Field]] = Expr.ofList {
      fields.map {
        case field: utils.ReaderField.Basic =>
          (field.name, field.idx, field.extractor.map(_._2), Nil, false)
        case field: utils.ReaderField.Extracted =>
          (field.name, field.idx, Some(field.lambda), field.extractors.map((name, _) => (name -> labelsToIndices.get(name))), field.reader)
      }.map { case (name, idx, lambda, dependencies, reader) =>
        '{
          JsonReaderProductConfigParsed.Field(
            name = ${Expr(name)},
            idx = ${Expr(idx)},
            function = ${lambda.map(lambda => '{ Some(${ lambda }) }).getOrElse('{None})},
            dependencies = ${Expr.ofList(dependencies.map((name, idx) => '{ ${ Expr(name) } -> ${Expr(idx)} }))},
            extractReader = ${Expr(reader)}
          )
        }
      }
    }

    '{
      JsonReaderProductConfigParsed(
        defaultValues = ${defaults.exprOfMap[String, Any]},
        fields = ${sortedFields},
        readers = ${readers},
        requiredLabels = ${requiredLabels.exprOfSet},
        fieldsWithoutReaders = ${fieldsWithoutReader.exprOfSet},
        isStrict = ${Expr(isStrict)}
      )
    }


  def writeDiscriminator[T: Type](config: Expr[JsonConfig[T]])(using quotes: Quotes): Expr[(T, TokenWriter) => Unit] =
    val utils = new ConfigurationMacroUtils
    import utils.quotes.reflect.*
    val parsed = utils.parseSumConfig(config)
    parsed.discriminator match
      case Some(utils.DiscriminatorConfig(label, tpe, discriminators)) => tpe.asType match
        case '[discriminator] => '{ (value: T, tokenWriter: TokenWriter) =>
          summonInline[JsonWriter[discriminator]].write(
            name = ${Expr(label)},
            value = ${Select.unique('{value}.asTerm, label).asExprOf[discriminator]},
            tokenWriter = tokenWriter
          )
        }
      case None =>
        '{ (value: T, tokenWriter: TokenWriter) => }

  def deriveJsonReaderForSum[T: Type](config: Expr[JsonConfig[T]], readers: Expr[List[JsonReader[?]]])(using quotes: Quotes): Expr[JsonReader[T]] =
    val utils = new ConfigurationMacroUtils
    import utils.quotes.reflect.*
    val tpe = TypeRepr.of[T]

    val parsed = utils.parseSumConfig(config)

    parsed.discriminator match
      case Some(utils.DiscriminatorConfig(label, tpe, discriminators)) => tpe.asType match
        case '[discriminator]  =>
          val allReadersExpr: Expr[List[JsonReader[T]]] =
            Typed(readers.asTerm, TypeTree.of[List[JsonReader[T]]]).asExprOf[List[JsonReader[T]]]

          val readersByDiscriminorsExpr = '{ ${Expr.ofList(discriminators)}.zip(${allReadersExpr}).toMap }

          '{
            lazy val readersByDiscriminator = ${ readersByDiscriminorsExpr }
            JsonReader.builder
              .addField[discriminator](
                name = ${Expr(label)},
                jsonReader = summonInline[JsonReader[discriminator]]
              )
              .selectReader[T] { discriminator =>
                readersByDiscriminator.getOrElse(
                  discriminator,
                  ReaderError.wrongJson(s"Unexpected discriminator found: $discriminator")(using FieldName(${Expr(label)}))
                )
              }
          }
      case None =>
        report.errorAndAbort("Discriminator is required to derive JsonReader for sum type. Use JsonConfig[T].discriminateBy(_.field)")











