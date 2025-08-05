package tethys.readers

import tethys.JsonReader
import tethys.readers.instances.{SelectingJsonReader, SelectingJsonReaderNoDefault, SimpleJsonReader, SimpleJsonReaderNoDefault}
import scala.annotation.targetName

object JsonReaderBuilder {
  @deprecated("use .field instead", "2025-08-01")
  def addField[B](name: String, jsonReader: JsonReader[B])(implicit
      readerDefaultValue: JsonReaderDefaultValue[B]
  ): JsonReaderBuilder1[B] = {
    new JsonReaderBuilder1[B](
      0,
      name,
      readerDefaultValue.defaultValue,
      jsonReader
    )
  }

  @deprecated
  def addField[B](name: String)(implicit
      readerDefaultValue: JsonReaderDefaultValue[B],
      jsonReader: JsonReader[B]
  ): JsonReaderBuilder1[B] = {
    new JsonReaderBuilder1[B](
      0,
      name,
      readerDefaultValue.defaultValue,
      jsonReader
    )
  }

  def field[B](
      name: String,
      jsonReader: JsonReader[B]
  ): JsonReaderBuilder1NoDefault[B] = {
    new JsonReaderBuilder1NoDefault[B](
      0,
      name,
      jsonReader
    )
  }

  @targetName("field2")
  def field[B](name: String)(implicit
      jsonReader: JsonReader[B]
  ): JsonReaderBuilder1NoDefault[B] = {
    new JsonReaderBuilder1NoDefault[B](
      0,
      name,
      jsonReader
    )
  }

  trait SingleJsonValueReader[A1] {
    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit

    private[JsonReaderBuilder] def value(extracted: Array[Any]): A1
  }

  final class JsonReaderBuilder1[A1] private[JsonReaderBuilder] (
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A1]
  ) extends SingleJsonValueReader[A1] {
    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A1](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): A1 = extracted(
      pos
    ).asInstanceOf[A1]

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder2[A1, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder2[A1, B] = {
      new JsonReaderBuilder2[A1, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](fun: A1 => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](fun: A1 => Res): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: A1 => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](fieldsArray, arr => fun(value(arr)), strict)
    }

    def selectReader[Res](fun: A1 => JsonReader[_ <: Res]): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReader[A1](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[A1, Res](simpleJsonReader)(fun)
    }
  }

  trait SingleJsonValueReaderNoDefault[A1] {
    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit

    private[JsonReaderBuilder] def value(extracted: Array[Any]): A1
  }


  final class JsonReaderBuilder1NoDefault[A1] private[JsonReaderBuilder] (
      pos: Int,
      name: String,
      jsonReader: JsonReader[A1]
  ) extends SingleJsonValueReaderNoDefault[A1] {
    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A1](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): A1 = extracted(
      pos
    ).asInstanceOf[A1]
    
    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder2NoDefault[A1, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder2NoDefault[A1, B] = {
      new JsonReaderBuilder2NoDefault[A1, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](fun: A1 => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](fun: A1 => Res): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: A1 => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](fieldsArray, arr => fun(value(arr)), strict)
    }

    def selectReader[Res](fun: A1 => JsonReader[_ <: Res]): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReaderNoDefault[A1](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[A1, Res](simpleJsonReader)(fun)
    }
  }
  
  final class JsonReaderBuilder2[A1, A2] private[JsonReaderBuilder] (
      prev: SingleJsonValueReader[A1],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A2]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A2](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (A1, A2) =
      (prev.value(extracted), extracted(pos).asInstanceOf[A2])

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder3[A1, A2, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder3[A1, A2, B] = {
      new JsonReaderBuilder3[A1, A2, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](fun: (A1, A2) => Res): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[(A1, A2)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReader[(A1, A2), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder2NoDefault[A1, A2] private[JsonReaderBuilder] (
      prev: SingleJsonValueReaderNoDefault[A1],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A2]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A2](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (A1, A2) =
      (prev.value(extracted), extracted(pos).asInstanceOf[A2])

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder3NoDefault[A1, A2, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder3NoDefault[A1, A2, B] = {
      new JsonReaderBuilder3NoDefault[A1, A2, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](fun: (A1, A2) => Res): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[(A1, A2)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReaderNoDefault[(A1, A2), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder3[A1, A2, A3] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder2[A1, A2],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A3]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A3](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (A1, A2, A3) =
      prev.value(extracted) match {
        case (a1, a2) => (a1, a2, extracted(pos).asInstanceOf[A3])
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder4[A1, A2, A3, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder4[A1, A2, A3, B] = {
      new JsonReaderBuilder4[A1, A2, A3, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2, A3) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](fun: (A1, A2, A3) => Res): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReader[(A1, A2, A3), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder3NoDefault[A1, A2, A3] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder2NoDefault[A1, A2],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A3]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A3](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (A1, A2, A3) =
      prev.value(extracted) match {
        case (a1, a2) => (a1, a2, extracted(pos).asInstanceOf[A3])
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder4NoDefault[A1, A2, A3, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder4NoDefault[A1, A2, A3, B] = {
      new JsonReaderBuilder4NoDefault[A1, A2, A3, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2, A3) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](fun: (A1, A2, A3) => Res): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[(A1, A2, A3)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReaderNoDefault[(A1, A2, A3), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder4[A1, A2, A3, A4] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder3[A1, A2, A3],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A4]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A4](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4) = prev.value(extracted) match {
      case (a1, a2, a3) => (a1, a2, a3, extracted(pos).asInstanceOf[A4])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder5[A1, A2, A3, A4, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder5[A1, A2, A3, A4, B] = {
      new JsonReaderBuilder5[A1, A2, A3, A4, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2, A3, A4) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReader[(A1, A2, A3, A4), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder4NoDefault[A1, A2, A3, A4] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder3NoDefault[A1, A2, A3],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A4]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A4](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4) = prev.value(extracted) match {
      case (a1, a2, a3) => (a1, a2, a3, extracted(pos).asInstanceOf[A4])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder5NoDefault[A1, A2, A3, A4, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder5NoDefault[A1, A2, A3, A4, B] = {
      new JsonReaderBuilder5NoDefault[A1, A2, A3, A4, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2, A3, A4) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[(A1, A2, A3, A4)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder5[
      A1,
      A2,
      A3,
      A4,
      A5
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder4[A1, A2, A3, A4],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A5]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A5](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5) = prev.value(extracted) match {
      case (a1, a2, a3, a4) => (a1, a2, a3, a4, extracted(pos).asInstanceOf[A5])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder6[A1, A2, A3, A4, A5, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder6[A1, A2, A3, A4, A5, B] = {
      new JsonReaderBuilder6[A1, A2, A3, A4, A5, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2, A3, A4, A5) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReader[(A1, A2, A3, A4, A5), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder6[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder5[A1, A2, A3, A4, A5],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A6]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A6](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5) =>
        (a1, a2, a3, a4, a5, extracted(pos).asInstanceOf[A6])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, B] = {
      new JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6), Res](simpleJsonReader)(
        fun
      )
    }
  }

  final class JsonReaderBuilder5NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder4NoDefault[A1, A2, A3, A4],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A5]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A5](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5) = prev.value(extracted) match {
      case (a1, a2, a3, a4) => (a1, a2, a3, a4, extracted(pos).asInstanceOf[A5])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder6NoDefault[A1, A2, A3, A4, A5, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder6NoDefault[A1, A2, A3, A4, A5, B] = {
      new JsonReaderBuilder6NoDefault[A1, A2, A3, A4, A5, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](fun: (A1, A2, A3, A4, A5) => Res): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4, A5), Res](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder6NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder5NoDefault[A1, A2, A3, A4, A5],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A6]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A6](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5) =>
        (a1, a2, a3, a4, a5, extracted(pos).asInstanceOf[A6])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder7NoDefault[A1, A2, A3, A4, A5, A6, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder7NoDefault[A1, A2, A3, A4, A5, A6, B] = {
      new JsonReaderBuilder7NoDefault[A1, A2, A3, A4, A5, A6, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6), Res](simpleJsonReader)(
        fun
      )
    }
  }

  final class JsonReaderBuilder7[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder6[A1, A2, A3, A4, A5, A6],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A7]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A7](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6) =>
        (a1, a2, a3, a4, a5, a6, extracted(pos).asInstanceOf[A7])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, B] = {
      new JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7), Res](
        simpleJsonReader
      )(fun)
    }
  }

    final class JsonReaderBuilder7NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder6NoDefault[A1, A2, A3, A4, A5, A6],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A7]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A7](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6) =>
        (a1, a2, a3, a4, a5, a6, extracted(pos).asInstanceOf[A7])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder8NoDefault[A1, A2, A3, A4, A5, A6, A7, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder8NoDefault[A1, A2, A3, A4, A5, A6, A7, B] = {
      new JsonReaderBuilder8NoDefault[A1, A2, A3, A4, A5, A6, A7, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7)](
        fieldsArray,
        arr => value(arr),
        strict = false
      )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder8[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder7[A1, A2, A3, A4, A5, A6, A7],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A8]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A8](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7) =>
        (a1, a2, a3, a4, a5, a6, a7, extracted(pos).asInstanceOf[A8])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, B] = {
      new JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7, A8)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder8NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder7NoDefault[A1, A2, A3, A4, A5, A6, A7],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A8]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A8](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7) =>
        (a1, a2, a3, a4, a5, a6, a7, extracted(pos).asInstanceOf[A8])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder9NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder9NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, B] = {
      new JsonReaderBuilder9NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7, A8)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder9[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder8[A1, A2, A3, A4, A5, A6, A7, A8],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A9]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A9](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8) =>
        (a1, a2, a3, a4, a5, a6, a7, a8, extracted(pos).asInstanceOf[A9])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B] = {
      new JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder9NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder8NoDefault[A1, A2, A3, A4, A5, A6, A7, A8],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A9]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A9](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8) =>
        (a1, a2, a3, a4, a5, a6, a7, a8, extracted(pos).asInstanceOf[A9])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder10NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder10NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, B] = {
      new JsonReaderBuilder10NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8, A9)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8, A9), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder10[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder9[A1, A2, A3, A4, A5, A6, A7, A8, A9],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A10]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A10](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9) =>
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, extracted(pos).asInstanceOf[A10])
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] = {
      new JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder10NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder9NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A10]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A10](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) = prev.value(extracted) match {
      case (a1, a2, a3, a4, a5, a6, a7, a8, a9) =>
        (a1, a2, a3, a4, a5, a6, a7, a8, a9, extracted(pos).asInstanceOf[A10])
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder11NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder11NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B] = {
      new JsonReaderBuilder11NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10), Res](
        simpleJsonReader
      )(fun)
    }
  }

  final class JsonReaderBuilder11[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A11]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A11](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            extracted(pos).asInstanceOf[A11]
          )
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] = {
      new JsonReaderBuilder12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
        this,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReader[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder11NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder10NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A11]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A11](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            extracted(pos).asInstanceOf[A11]
          )
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder12NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder12NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B] = {
      new JsonReaderBuilder12NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, B](
        this,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader =
        new SimpleJsonReaderNoDefault[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)](
          fieldsArray,
          arr => value(arr),
          strict = false
        )
      new SelectingJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder12[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A12]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A12](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            extracted(pos).asInstanceOf[A12]
          )
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder13[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder13[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      B
    ] = {
      new JsonReaderBuilder13[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder12NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder11NoDefault[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A12]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A12](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            extracted(pos).asInstanceOf[A12]
          )
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder13NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder13NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      B
    ] = {
      new JsonReaderBuilder13NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder13[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder12[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A13]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A13](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            extracted(pos).asInstanceOf[A13]
          )
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder14[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder14[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      B
    ] = {
      new JsonReaderBuilder14[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder13NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder12NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A13]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A13](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            extracted(pos).asInstanceOf[A13]
          )
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder14NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder14NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      B
    ] = {
      new JsonReaderBuilder14NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder14[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder13[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A14]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A14](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            extracted(pos).asInstanceOf[A14]
          )
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder15[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder15[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      B
    ] = {
      new JsonReaderBuilder15[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder14NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder13NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A14]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A14](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            extracted(pos).asInstanceOf[A14]
          )
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder15NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder15NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      B
    ] = {
      new JsonReaderBuilder15NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder15[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder14[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A15]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A15](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            extracted(pos).asInstanceOf[A15]
          )
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder16[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder16[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      B
    ] = {
      new JsonReaderBuilder16[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder15NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder14NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A15]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A15](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) =
      prev.value(extracted) match {
        case (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            extracted(pos).asInstanceOf[A15]
          )
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder16NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder16NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      B
    ] = {
      new JsonReaderBuilder16NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder16[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder15[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A16]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A16](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
      prev.value(extracted) match {
        case (
              a1,
              a2,
              a3,
              a4,
              a5,
              a6,
              a7,
              a8,
              a9,
              a10,
              a11,
              a12,
              a13,
              a14,
              a15
            ) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            extracted(pos).asInstanceOf[A16]
          )
      }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder17[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder17[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      B
    ] = {
      new JsonReaderBuilder17[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder16NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder15NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A16]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A16](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(
        extracted: Array[Any]
    ): (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) =
      prev.value(extracted) match {
        case (
              a1,
              a2,
              a3,
              a4,
              a5,
              a6,
              a7,
              a8,
              a9,
              a10,
              a11,
              a12,
              a13,
              a14,
              a15
            ) =>
          (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            extracted(pos).asInstanceOf[A16]
          )
      }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder17NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder17NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      B
    ] = {
      new JsonReaderBuilder17NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder17[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder16[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A17]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A17](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          extracted(pos).asInstanceOf[A17]
        )
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder18[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder18[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      B
    ] = {
      new JsonReaderBuilder18[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder17NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder16NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A17]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A17](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          extracted(pos).asInstanceOf[A17]
        )
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder18NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder18NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      B
    ] = {
      new JsonReaderBuilder18NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder18[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder17[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A18]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A18](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          extracted(pos).asInstanceOf[A18]
        )
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder19[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder19[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      B
    ] = {
      new JsonReaderBuilder19[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder18NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder17NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A18]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A18](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          extracted(pos).asInstanceOf[A18]
        )
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder19NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder19NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      B
    ] = {
      new JsonReaderBuilder19NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder19[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder18[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A19]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A19](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          extracted(pos).asInstanceOf[A19]
        )
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder20[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder20[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      B
    ] = {
      new JsonReaderBuilder20[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder19NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder18NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A19]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A19](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          extracted(pos).asInstanceOf[A19]
        )
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder20NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder20NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      B
    ] = {
      new JsonReaderBuilder20NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder20[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder19[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A20]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A20](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          extracted(pos).asInstanceOf[A20]
        )
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder21[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder21[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      B
    ] = {
      new JsonReaderBuilder21[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19,
                A20
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder20NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder19NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A20]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A20](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          extracted(pos).asInstanceOf[A20]
        )
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder21NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder21NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      B
    ] = {
      new JsonReaderBuilder21NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19,
                A20
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder21[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder20[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A21]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A21](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19,
            a20
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          a20,
          extracted(pos).asInstanceOf[A21]
        )
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder22[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder22[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      B
    ] = {
      new JsonReaderBuilder22[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        B
      ](this, pos + 1, name, readerDefaultValue.defaultValue, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19,
                A20,
                A21
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder21NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder20NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A21]
  ) {

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A21](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19,
            a20
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          a20,
          extracted(pos).asInstanceOf[A21]
        )
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder22NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder22NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      B
    ] = {
      new JsonReaderBuilder22NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        B
      ](this, pos + 1, name, jsonReader)
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19,
                A20,
                A21
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder22[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder21[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21
      ],
      pos: Int,
      name: String,
      defaultValue: Any,
      jsonReader: JsonReader[A22]
  ) {
    self =>

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReader.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) =
        SimpleJsonReader.FieldDefinition[A22](name, defaultValue, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19,
            a20,
            a21
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          a20,
          a21,
          extracted(pos).asInstanceOf[A22]
        )
    }

    @deprecated
    def addField[B](name: String, jsonReader: JsonReader[B])(implicit
        readerDefaultValue: JsonReaderDefaultValue[B]
    ): JsonReaderBuilder2[
      (
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          A22
      ),
      B
    ] = {
      addField[B](name)(readerDefaultValue, jsonReader)
    }

    @deprecated
    def addField[B](name: String)(implicit
        readerDefaultValue: JsonReaderDefaultValue[B],
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder2[
      (
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          A22
      ),
      B
    ] = {
      val singleJsonValueReader: SingleJsonValueReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        )
      ] = {
        new SingleJsonValueReader[
          (
              A1,
              A2,
              A3,
              A4,
              A5,
              A6,
              A7,
              A8,
              A9,
              A10,
              A11,
              A12,
              A13,
              A14,
              A15,
              A16,
              A17,
              A18,
              A19,
              A20,
              A21,
              A22
          )
        ] {
          private[JsonReaderBuilder] override def fields(
              arr: Array[SimpleJsonReader.FieldDefinition[_]]
          ): Unit = self.fields(arr)

          override def value(extracted: Array[Any]): (
              A1,
              A2,
              A3,
              A4,
              A5,
              A6,
              A7,
              A8,
              A9,
              A10,
              A11,
              A12,
              A13,
              A14,
              A15,
              A16,
              A17,
              A18,
              A19,
              A20,
              A21,
              A22
          ) = {
            self.value(extracted)
          }
        }
      }
      new JsonReaderBuilder2[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ),
        B
      ](
        singleJsonValueReader,
        pos + 1,
        name,
        readerDefaultValue.defaultValue,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReader[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19,
                A20,
                A21,
                A22
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReader.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReader[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

  final class JsonReaderBuilder22NoDefault[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22
  ] private[JsonReaderBuilder] (
      prev: JsonReaderBuilder21NoDefault[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21
      ],
      pos: Int,
      name: String,
      jsonReader: JsonReader[A22]
  ) {
    self =>

    private[JsonReaderBuilder] def fields(
        arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
    ): Unit = {
      prev.fields(arr)
      arr(pos) = SimpleJsonReaderNoDefault.FieldDefinition[A22](name, jsonReader)
    }

    private[JsonReaderBuilder] def value(extracted: Array[Any]): (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22
    ) = prev.value(extracted) match {
      case (
            a1,
            a2,
            a3,
            a4,
            a5,
            a6,
            a7,
            a8,
            a9,
            a10,
            a11,
            a12,
            a13,
            a14,
            a15,
            a16,
            a17,
            a18,
            a19,
            a20,
            a21
          ) =>
        (
          a1,
          a2,
          a3,
          a4,
          a5,
          a6,
          a7,
          a8,
          a9,
          a10,
          a11,
          a12,
          a13,
          a14,
          a15,
          a16,
          a17,
          a18,
          a19,
          a20,
          a21,
          extracted(pos).asInstanceOf[A22]
        )
    }

    def field[B](
        name: String,
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder2NoDefault[
      (
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          A22
      ),
      B
    ] = {
      field[B](name)(jsonReader)
    }

    @targetName("field1")
    def field[B](name: String)(implicit
        jsonReader: JsonReader[B]
    ): JsonReaderBuilder2NoDefault[
      (
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          A22
      ),
      B
    ] = {
      val singleJsonValueReader: SingleJsonValueReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        )
      ] = {
        new SingleJsonValueReaderNoDefault[
          (
              A1,
              A2,
              A3,
              A4,
              A5,
              A6,
              A7,
              A8,
              A9,
              A10,
              A11,
              A12,
              A13,
              A14,
              A15,
              A16,
              A17,
              A18,
              A19,
              A20,
              A21,
              A22
          )
        ] {
          private[JsonReaderBuilder] override def fields(
              arr: Array[SimpleJsonReaderNoDefault.FieldDefinition[_]]
          ): Unit = self.fields(arr)

          override def value(extracted: Array[Any]): (
              A1,
              A2,
              A3,
              A4,
              A5,
              A6,
              A7,
              A8,
              A9,
              A10,
              A11,
              A12,
              A13,
              A14,
              A15,
              A16,
              A17,
              A18,
              A19,
              A20,
              A21,
              A22
          ) = {
            self.value(extracted)
          }
        }
      }
      new JsonReaderBuilder2NoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ),
        B
      ](
        singleJsonValueReader,
        pos + 1,
        name,
        jsonReader
      )
    }

    def buildReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = false, fun)
    }

    def buildStrictReader[Res](
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ) => Res
    ): JsonReader[Res] = {
      buildReader(strict = true, fun)
    }

    private def buildReader[Res](
        strict: Boolean,
        fun: (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ) => Res
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      new SimpleJsonReaderNoDefault[Res](
        fieldsArray,
        arr => fun.tupled(value(arr)),
        strict
      )
    }

    def selectReader[Res](
        fun: (
            (
                A1,
                A2,
                A3,
                A4,
                A5,
                A6,
                A7,
                A8,
                A9,
                A10,
                A11,
                A12,
                A13,
                A14,
                A15,
                A16,
                A17,
                A18,
                A19,
                A20,
                A21,
                A22
            )
        ) => JsonReader[_ <: Res]
    ): JsonReader[Res] = {
      val fieldsArray = new Array[SimpleJsonReaderNoDefault.FieldDefinition[_]](pos + 1)
      fields(fieldsArray)
      val simpleJsonReader = new SimpleJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        )
      ](fieldsArray, arr => value(arr), strict = false)
      new SelectingJsonReaderNoDefault[
        (
            A1,
            A2,
            A3,
            A4,
            A5,
            A6,
            A7,
            A8,
            A9,
            A10,
            A11,
            A12,
            A13,
            A14,
            A15,
            A16,
            A17,
            A18,
            A19,
            A20,
            A21,
            A22
        ),
        Res
      ](simpleJsonReader)(fun)
    }
  }

}
