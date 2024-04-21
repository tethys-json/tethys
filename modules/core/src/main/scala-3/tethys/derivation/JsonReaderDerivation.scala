package tethys.derivation

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.JsonReader

import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue, constValueTuple, summonFrom}


private [tethys] trait JsonReaderDerivation:
  inline def derived[A](using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    new JsonReader[A]:
      override def read(it: TokenIterator)(implicit fieldName: FieldName) =
        if !it.currentToken().isObjectStart then
          ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)
        else
          it.nextToken()
          val labels = constValueTuple[mirror.MirroredElemLabels].toArray.collect { case s: String => s }
          val readersByLabels = labels.zip(summonJsonReaders[A, mirror.MirroredElemTypes].zipWithIndex).toMap
          val defaults = getOptionsByIndex[mirror.MirroredElemTypes]().toMap ++ Defaults.collectFrom[A]
          val optionalLabels = defaults.keys.map(labels(_))

          val collectedValues = scala.collection.mutable.Map.from[Int, Any](defaults)
          val missingFields = scala.collection.mutable.Set.from(labels) -- optionalLabels

          while (!it.currentToken().isObjectEnd)
            val jsonName = it.fieldName()
            it.nextToken()
            val currentIt = it.collectExpression()
            readersByLabels.get(jsonName).foreach { (reader, idx) =>
              val value: Any = reader.read(currentIt.copy())(fieldName.appendFieldName(jsonName))
              collectedValues += idx -> value
              missingFields -= jsonName
            }
            
          it.nextToken()

          if (missingFields.nonEmpty)
            ReaderError.wrongJson("Can not extract fields from json: " + missingFields.mkString(", "))
          else
            mirror.fromProduct:
              new Product:
                override def productArity = labels.length
                override def productElement(n: Int) = collectedValues(n)
                override def canEqual(that: Any) =
                  that match
                    case that: Product if that.productArity == productArity => true
                    case _ => false

  private inline def summonJsonReaders[T, Elems <: Tuple]: List[JsonReader[?]] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Nil
      case _: (elem *: elems) =>
        deriveOrSummon[T, elem] :: summonJsonReaders[T, elems]

  private inline def deriveOrSummon[T, Elem]: JsonReader[Elem] =
    inline erasedValue[Elem] match
      case _: T =>
        deriveRec[T, Elem]
      case _ =>
        summonInline[JsonReader[Elem]]

  private inline def deriveRec[T, Elem]: JsonReader[Elem] =
    inline erasedValue[T] match
      case _: Elem =>
        scala.compiletime.error("Recursive derivation is not possible")
      case _ =>
        JsonReader.derived[Elem](using summonInline[Mirror.ProductOf[Elem]])


  private inline def getOptionsByIndex[Elems <: Tuple](idx: Int = 0): List[(Int, None.type)] =
    inline erasedValue[Elems] match
      case _: EmptyTuple =>
        Nil
      case _: (Option[?] *: elems) =>
        idx -> None :: getOptionsByIndex[elems](idx + 1)
      case _: (_ *: elems) =>
        getOptionsByIndex[elems](idx + 1)


