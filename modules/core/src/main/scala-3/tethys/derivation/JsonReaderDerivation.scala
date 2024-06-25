package tethys.derivation

import tethys.commons.TokenNode
import tethys.readers.tokens.{QueueIterator, TokenIterator}
import tethys.readers.{FieldName, ReaderError}
import tethys.{JsonReader, JsonConfig}

import scala.collection.mutable
import scala.deriving.Mirror
import scala.compiletime.{constValue, constValueTuple, erasedValue, summonFrom, summonInline}


private [tethys]
trait JsonReaderDerivation extends JsonReaderConfiguration:

  inline def derived[A](inline config: JsonReader.ProductConfig[A])(using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    deriveJsonReaderForProduct(config)
  
  inline def derived[A](inline config: JsonConfig[A])(using mirror: Mirror.SumOf[A]): JsonReader[A] =
    Derivation.deriveJsonReaderForSum(config, summonJsonReadersForSum[A, mirror.MirroredElemTypes](config))


  inline def derived[A](using mirror: Mirror.Of[A]): JsonReader[A] =
    inline mirror match
      case given Mirror.ProductOf[A] =>
        derived(
          summonFrom[JsonReader.ProductConfig[A]] {
            case config: JsonReader.ProductConfig[A] => config
            case _ => JsonReader.configure[A]
          }
        )
      case given Mirror.SumOf[A] =>
        derived(
          summonFrom[JsonConfig[A]] {
            case config: JsonConfig[A] => config
            case _ => JsonConfig.configure[A]
          }
        )


  private inline def deriveJsonReaderForProduct[A](inline config: JsonReader.ProductConfig[A])(using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    new JsonReader[A]:
      given JsonReader[A] = this
      lazy val configuration: JsonReaderProductConfigParsed = Derivation.parseJsonReaderProductConfig[A](config)

      def read(it: TokenIterator)(implicit fieldName: FieldName) =
        if !it.currentToken().isObjectStart then
          ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)
        else
          it.nextToken()
          val collectedValues: mutable.Map[String, Any] = mutable.Map.empty.withDefault(configuration.defaultValues(_))
          val missingFields: mutable.Set[String] = mutable.Set.from(configuration.requiredLabels)
          val resultFields = mutable.Map.empty[Int, Any]
          val fieldsForExtractedReader = mutable.Map.empty[String, TokenIterator]
          val fieldsWithoutReaders: mutable.Set[String] = mutable.Set.from(configuration.fieldsWithoutReaders)

          while (!it.currentToken().isObjectEnd)
            val jsonName = it.fieldName()
            it.nextToken()
            val currentIt = it.collectExpression()

            configuration.readers.get(jsonName) match
              case Some(reader) =>
                collectedValues += jsonName -> reader.read(currentIt.copy())(fieldName.appendFieldName(jsonName))
                missingFields -= jsonName

              case None if fieldsWithoutReaders.contains(jsonName) =>
                fieldsForExtractedReader.update(jsonName, currentIt)
                missingFields -= jsonName
                fieldsWithoutReaders -= jsonName

              case None if configuration.isStrict =>
                val expectedNames = (collectedValues.keySet ++ missingFields ++ configuration.defaultValues.keySet).mkString("'", "', '", "'")
                ReaderError.wrongJson(s"unexpected field '$jsonName', expected one of $expectedNames")

              case None =>

          it.nextToken()

          if (missingFields.nonEmpty)
            ReaderError.wrongJson("Can not extract fields from json: " + missingFields.mkString(", "))

          for JsonReaderProductConfigParsed.Field(name, idx, function, dependencies, extractReader) <- configuration.fields do
            dependencies match
              case Nil =>
                resultFields += idx -> function.fold(collectedValues(name))(_.apply(collectedValues(name)))

              case dependencies =>
                val value = dependencies match
                  case (depName, depIdx) :: Nil =>
                    function.get.apply(depIdx.fold(collectedValues(depName))(resultFields(_)))
                  case _ =>
                    function.get.apply {
                      Tuple.fromArray {
                        dependencies
                          .map((depName, depIdx) => depIdx.fold(collectedValues(depName))(resultFields(_)))
                          .toArray
                      }
                    }

                if !extractReader then
                  resultFields += idx -> value
                else
                  val read: TokenIterator => Any = value.asInstanceOf[JsonReader[Any]].read(_)(fieldName.appendFieldName(name))
                  fieldsForExtractedReader.get(name) match
                    case Some(iterator) =>
                      resultFields += idx -> read(iterator)
                    case _ =>
                      try
                        resultFields += idx -> read(QueueIterator(List(TokenNode.NullValueNode)))
                        fieldsWithoutReaders -= name
                      catch
                        case _: ReaderError =>
                      


          if (fieldsWithoutReaders.nonEmpty)
            ReaderError.wrongJson("Can not extract fields from json: " + fieldsWithoutReaders.mkString(", "))

          mirror.fromProduct:
            new Product:
              def productArity = resultFields.size
              def productElement(n: Int) = resultFields(n)
              def canEqual(that: Any) = that match
                case that: Product if that.productArity == productArity => true
                case _ => false


  private inline def summonJsonReadersForSum[T, Elems <: Tuple](inline config: JsonConfig[T]): List[JsonReader[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Nil
      case _: (elem *: elems) =>
        summonOrDeriveJsonReaderForSum[T, elem](config) :: summonJsonReadersForSum[T, elems](config)

  private inline def summonOrDeriveJsonReaderForSum[T, Elem](inline config: JsonConfig[T]): JsonReader[Elem] =
    summonFrom[JsonReader[Elem]] {
      case reader: JsonReader[Elem] =>
        reader
      case _ =>
        JsonReader.derived[Elem](using summonInline[Mirror.ProductOf[Elem]])
    }

private[derivation]
object JsonReaderDerivation:

  inline def summonOrDeriveJsonReaderForField[T, Field]: JsonReader[Field] =
    summonFrom[Mirror.Of[Field]] {
      case mirror: Mirror.Of[Field] =>
        summonFrom[JsonReader[Field]] {
          case writer: JsonReader[Field] =>
            writer
          case _ =>
            deriveRec[T, Field]
        }
      case _ =>
        summonInline[JsonReader[Field]]
    }


  inline def deriveRec[T, Field]: JsonReader[Field] =
    inline erasedValue[T] match
      case _: Field =>
        scala.compiletime.error("Recursive derivation is not possible")
      case _ =>
        JsonReader.derived[Field](using summonInline[Mirror.ProductOf[Field]])