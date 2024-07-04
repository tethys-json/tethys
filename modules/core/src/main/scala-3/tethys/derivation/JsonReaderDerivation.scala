package tethys.derivation

import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonObjectWriter, JsonReader}
import tethys.writers.tokens.TokenWriter

import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValue, constValueTuple}


private [tethys] trait JsonReaderDerivation:
  inline def derived[A](using mirror: Mirror.ProductOf[A]): JsonReader[A] =
    new JsonReader[A]:
      override def read(it: TokenIterator)(implicit fieldName: FieldName) =
        if !it.currentToken().isObjectStart then
          ReaderError.wrongJson("Expected object start but found: " + it.currentToken().toString)
        else
          it.nextToken()
          val labels = constValueTuple[mirror.MirroredElemLabels].toArray.collect { case s: String => s }
          val readers = summonAll[mirror.MirroredElemTypes]
          val readersByLabels = labels.zip(readers.zipWithIndex).toMap

          val readValues = scala.collection.mutable.Map.empty[Int, Any]
          val missingFields = scala.collection.mutable.Set.from(labels)

          while (!it.currentToken().isObjectEnd)
            val jsonName = it.fieldName()
            it.nextToken()
            val currentIt = it.collectExpression()
            readersByLabels.get(jsonName).foreach { (reader, idx) =>
              val value: Any = reader.read(currentIt.copy())(fieldName.appendFieldName(jsonName))
              readValues += idx -> value
              missingFields -= jsonName
            }
            
          it.nextToken()

          if (missingFields.nonEmpty)
            ReaderError.wrongJson("Can not extract fields from json: " + missingFields.mkString(", "))
          else
            mirror.fromProduct:
              new Product:
                override def productArity = labels.length
                override def productElement(n: Int) = readValues(n)
                override def canEqual(that: Any) =
                  that match
                    case that: Product if that.productArity == productArity => true
                    case _ => false





  private inline def summonAll[T <: Tuple]: List[JsonReader[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JsonReader[t]] :: summonAll[ts]

  private inline def getLabels[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case EmptyTuple => Nil
      case _: (t *: ts) =>
        constValue[t].asInstanceOf[String] :: getLabels[ts]




