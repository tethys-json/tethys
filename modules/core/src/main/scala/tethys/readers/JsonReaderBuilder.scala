package tethys.readers

import tethys.JsonReader
import tethys.readers.instances.{SelectingJsonReader, SimpleJsonReader}

import scala.reflect.ClassTag

object JsonReaderBuilder {
  def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder1[B] = {
    new JsonReaderBuilder1[B](name, jsonReader)
  }

  trait SingleJsonValueReader[A1] {
    def fields(): Map[String, JsonReader[_]]
    def value(extracted: Map[String, Any]): A1
  }

  final class JsonReaderBuilder1[A1] private[JsonReaderBuilder](name: String, jsonReader: JsonReader[A1]) extends SingleJsonValueReader[A1] {
    def fields(): Map[String, JsonReader[_]] = Map(name -> jsonReader)

    def value(extracted: Map[String, Any]): A1 = extracted(name).asInstanceOf[A1]

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder2[A1, B] = {
      new JsonReaderBuilder2[A1, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: A1 => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun(value(m)))
    }

    def selectReader[Res](fun: PartialFunction[A1, JsonReader[_ <: Res]])(implicit Res: ClassTag[Res], A1: ClassTag[A1]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[A1](fields())(m => value(m))
      new SelectingJsonReader[A1, Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder2[A1, A2] private[JsonReaderBuilder](prev: SingleJsonValueReader[A1], name: String, jsonReader: JsonReader[A2]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2) = (prev.value(extracted), extracted(name).asInstanceOf[A2])

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder3[A1, A2, B] = {
      new JsonReaderBuilder3[A1, A2, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder3[A1, A2, A3] private[JsonReaderBuilder](prev: JsonReaderBuilder2[A1, A2], name: String, jsonReader: JsonReader[A3]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3) = prev.value(extracted) match {
      case (a1, a2) => (a1, a2, extracted(name).asInstanceOf[A3])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder4[A1, A2, A3, B] = {
      new JsonReaderBuilder4[A1, A2, A3, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder4[A1, A2, A3, A4] private[JsonReaderBuilder](prev: JsonReaderBuilder3[A1, A2, A3], name: String, jsonReader: JsonReader[A4]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4) = prev.value(extracted) match {
      case (a1, a2, a3) => (a1, a2, a3, extracted(name).asInstanceOf[A4])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder5[A1, A2, A3, A4, B] = {
      new JsonReaderBuilder5[A1, A2, A3, A4, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder5[A1, A2, A3, A4, A5] private[JsonReaderBuilder](prev: JsonReaderBuilder4[A1, A2, A3, A4], name: String, jsonReader: JsonReader[A5]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5) = prev.value(extracted) match {
      case (a1, a2, a3, a4) => (a1, a2, a3, a4, extracted(name).asInstanceOf[A5])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder6[A1, A2, A3, A4, A5, B] = {
      new JsonReaderBuilder6[A1, A2, A3, A4, A5, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder6[A1, A2, A3, A4, A5, A6] private[JsonReaderBuilder](prev: JsonReaderBuilder5[A1, A2, A3, A4, A5], name: String, jsonReader: JsonReader[A6]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5) => (a1, a2, a3, a4, a5, extracted(name).asInstanceOf[A6])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, B] = {
      new JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, A7] private[JsonReaderBuilder](prev: JsonReaderBuilder6[A1, A2, A3, A4, A5, A6], name: String, jsonReader: JsonReader[A7]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6) => (a1, a2, a3, a4, a5, a6, extracted(name).asInstanceOf[A7])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, B] = {
      new JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, A8] private[JsonReaderBuilder](prev: JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, A7], name: String, jsonReader: JsonReader[A8]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7) => (a1, a2, a3, a4, a5, a6, a7, extracted(name).asInstanceOf[A8])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, B] = {
      new JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, A9] private[JsonReaderBuilder](prev: JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, A8], name: String, jsonReader: JsonReader[A9]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8) => (a1, a2, a3, a4, a5, a6, a7, a8, extracted(name).asInstanceOf[A9])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B] = {
      new JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10] private[JsonReaderBuilder](prev: JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, A9], name: String, jsonReader: JsonReader[A10]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, extracted(name).asInstanceOf[A10])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] = {
      new JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11] private[JsonReaderBuilder](prev: JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10], name: String, jsonReader: JsonReader[A11]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, extracted(name).asInstanceOf[A11])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] = {
      new JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12] private[JsonReaderBuilder](prev: JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11], name: String, jsonReader: JsonReader[A12]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, extracted(name).asInstanceOf[A12])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B] = {
      new JsonReaderBuilder13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13] private[JsonReaderBuilder](prev: JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12], name: String, jsonReader: JsonReader[A13]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, extracted(name).asInstanceOf[A13])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B] = {
      new JsonReaderBuilder14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14] private[JsonReaderBuilder](prev: JsonReaderBuilder13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13], name: String, jsonReader: JsonReader[A14]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, extracted(name).asInstanceOf[A14])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B] = {
      new JsonReaderBuilder15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15] private[JsonReaderBuilder](prev: JsonReaderBuilder14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14], name: String, jsonReader: JsonReader[A15]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, extracted(name).asInstanceOf[A15])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B] = {
      new JsonReaderBuilder16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16] private[JsonReaderBuilder](prev: JsonReaderBuilder15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15], name: String, jsonReader: JsonReader[A16]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, extracted(name).asInstanceOf[A16])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B] = {
      new JsonReaderBuilder17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17] private[JsonReaderBuilder](prev: JsonReaderBuilder16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16], name: String, jsonReader: JsonReader[A17]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, extracted(name).asInstanceOf[A17])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B] = {
      new JsonReaderBuilder18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18] private[JsonReaderBuilder](prev: JsonReaderBuilder17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17], name: String, jsonReader: JsonReader[A18]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, extracted(name).asInstanceOf[A18])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B] = {
      new JsonReaderBuilder19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19] private[JsonReaderBuilder](prev: JsonReaderBuilder18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18], name: String, jsonReader: JsonReader[A19]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, extracted(name).asInstanceOf[A19])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B] = {
      new JsonReaderBuilder20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20] private[JsonReaderBuilder](prev: JsonReaderBuilder19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19], name: String, jsonReader: JsonReader[A20]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, extracted(name).asInstanceOf[A20])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B] = {
      new JsonReaderBuilder21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21] private[JsonReaderBuilder](prev: JsonReaderBuilder20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20], name: String, jsonReader: JsonReader[A21]) {

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, extracted(name).asInstanceOf[A21])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B] = {
      new JsonReaderBuilder22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, B](this, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22] private[JsonReaderBuilder](prev: JsonReaderBuilder21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21], name: String, jsonReader: JsonReader[A22]) {
    self =>

    def fields(): Map[String, JsonReader[_]] = prev.fields() + (name -> jsonReader)

    def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) => (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, extracted(name).asInstanceOf[A22])
    }

    def addField[B](name: String)(implicit jsonReader: JsonReader[B]): JsonReaderBuilder2[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), B] = {
      val singleJsonValueReader: SingleJsonValueReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] = {
        new SingleJsonValueReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)] {
          override def fields(): Map[String, JsonReader[_]] = self.fields()

          override def value(extracted: Map[String, Any]): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) = {
            self.value(extracted)
          }
        }
      }
      new JsonReaderBuilder2[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), B](singleJsonValueReader, name, jsonReader)
    }

    def buildReader[Res: ClassTag](fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Res): JsonReader[Res] = {
      new SimpleJsonReader[Res](fields())(m => fun.tupled(value(m)))
    }

    def selectReader[Res: ClassTag](fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)) => JsonReader[_ <: Res]): JsonReader[Res] = {
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)](fields())(m => value(m))
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22), Res](simpleJsonReader)(fun)
    }
  }
}
