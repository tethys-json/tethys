package tethys.json4s.ast

import org.json4s.JsonAST
import org.json4s.JsonAST._
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.writers.tokens.TokenWriter

trait Json4sSupport {

  implicit lazy val json4sJNothingWriter: JsonWriter[JNothing.type] = new JsonWriter[JsonAST.JNothing.type] {
    override def write(name: String, value: JsonAST.JNothing.type, tokenWriter: TokenWriter): Unit = ()
    override def write(value: JsonAST.JNothing.type, tokenWriter: TokenWriter): Unit = ()
  }
  implicit lazy val json4sJNullWriter: JsonWriter[JNull.type] = new JsonWriter[JsonAST.JNull.type] {
    override def write(value: JsonAST.JNull.type, tokenWriter: TokenWriter): Unit = tokenWriter.writeNull()
  }
  implicit lazy val json4sJStringWriter: JsonWriter[JString] = JsonWriter[String].contramap(_.s)
  implicit lazy val json4sJDoubleWriter: JsonWriter[JDouble] = JsonWriter[Double].contramap(_.num)
  implicit lazy val json4sJDecimalWriter: JsonWriter[JDecimal] = JsonWriter[BigDecimal].contramap(_.num)
  implicit lazy val json4sJLongWriter: JsonWriter[JLong] = JsonWriter[Long].contramap(_.num)
  implicit lazy val json4sJIntWriter: JsonWriter[JInt] = JsonWriter[BigInt].contramap(_.num)
  implicit lazy val json4sJBoolWriter: JsonWriter[JBool] = JsonWriter[Boolean].contramap(_.value)
  implicit lazy val json4sJObjectWriter: JsonWriter[JObject] = new JsonObjectWriter[JObject] {
    override def writeValues(value: JObject, tokenWriter: TokenWriter): Unit = value.obj.foreach { t =>
      json4sJValueWriter.write(t._1, t._2, tokenWriter)
    }
  }
  implicit lazy val json4sJArrayWriter: JsonWriter[JArray] = JsonWriter[List[JValue]].contramap(_.arr)
  implicit lazy val json4sJSetWriter: JsonWriter[JSet] = JsonWriter[Set[JValue]].contramap(_.set)

  implicit lazy val json4sJValueWriter: JsonWriter[JValue] = new JsonWriter[JValue] {
    override def write(value: JValue, tokenWriter: TokenWriter): Unit = value match {
      case JNothing => JsonWriter[JNothing.type].write(JNothing, tokenWriter)
      case JNull => JsonWriter[JNull.type].write(JNull, tokenWriter)
      case x: JString => JsonWriter[JString].write(x, tokenWriter)
      case x: JDouble => JsonWriter[JDouble].write(x, tokenWriter)
      case x: JDecimal => JsonWriter[JDecimal].write(x, tokenWriter)
      case x: JLong => JsonWriter[JLong].write(x, tokenWriter)
      case x: JInt => JsonWriter[JInt].write(x, tokenWriter)
      case x: JBool => JsonWriter[JBool].write(x, tokenWriter)
      case x: JObject => JsonWriter[JObject].write(x, tokenWriter)
      case x: JArray => JsonWriter[JArray].write(x, tokenWriter)
      case x: JSet => JsonWriter[JSet].write(x, tokenWriter)
    }
  }

  implicit lazy val json4sJStringReader: JsonReader[JString] = JsonReader[String].map(JString(_))
  implicit lazy val json4sJDoubleReader: JsonReader[JDouble] = JsonReader[Double].map(JDouble(_))
  implicit lazy val json4sJDecimalReader: JsonReader[JDecimal] = JsonReader[BigDecimal].map(JDecimal(_))
  implicit lazy val json4sJLongReader: JsonReader[JLong] = JsonReader[Long].map(JLong(_))
  implicit lazy val json4sJIntReader: JsonReader[JInt] = JsonReader[BigInt].map(JInt(_))
  implicit lazy val json4sJBoolReader: JsonReader[JBool] = JsonReader[Boolean].map(b => if(b) JBool.True else JBool.False)
  implicit lazy val json4sJObjectReader: JsonReader[JObject] = new JsonReader[JObject] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): JObject = {
      if (!it.currentToken().isObjectStart) ReaderError.wrongJson(s"Expected object start but found: ${it.currentToken()}")
      else {
        it.next()
        val builder = List.newBuilder[(String, JValue)]
        while(!it.currentToken().isObjectEnd) {
          val token = it.currentToken()
          if(token.isFieldName) {
            val name = it.fieldName()
            val value = json4sJValueReader.read(it.next())(fieldName.appendFieldName(name))
            builder += name -> value
          } else {
            ReaderError.wrongJson(s"Expect end of object or field name but '$token' found")(fieldName)
          }
        }
        it.next()
        JObject(builder.result())
      }
    }
  }
  implicit lazy val json4sJArrayReader: JsonReader[JArray] = JsonReader[List[JValue]].map(JArray(_))
  implicit lazy val json4sJSetReader: JsonReader[JSet] = JsonReader[Set[JValue]].map(JSet(_))

  implicit lazy val json4sJValueReader: JsonReader[JValue] = new JsonReader[JValue] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): JValue = {
      val token = it.currentToken()
      if(token.isObjectStart) JsonReader[JObject].read(it)
      else if (token.isArrayStart) JsonReader[JArray].read(it)
      else if (token.isStringValue) JsonReader[JString].read(it)
      else if (token.isBooleanValue) JsonReader[JBool].read(it)
      else if (token.isNumberValue) JsonReader[Number].read(it) match {
        case x@(_: java.lang.Short | _: java.lang.Integer | _: java.lang.Long) => JLong(x.longValue())
        case x@(_: java.lang.Float | _: java.lang.Double) => JDouble(x.doubleValue())

        case x: java.math.BigInteger => JInt(x)
        case x: BigInt => JInt(x)

        case x: java.math.BigDecimal=> JDecimal(x)
        case x: BigDecimal => JDecimal(x)
        case x => JDecimal(x.doubleValue())
      }
      else if (token.isNullValue) {
        it.next()
        JNull
      }
      else ReaderError.wrongJson(s"Unexpected token found: $token")(fieldName)
    }
  }
}
