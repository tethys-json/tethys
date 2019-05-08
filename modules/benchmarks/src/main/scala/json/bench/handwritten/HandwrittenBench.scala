package json.bench.handwritten

import com.fasterxml.jackson.core._
import com.fasterxml.jackson.core.io.{IOContext, SegmentedStringWriter}
import com.fasterxml.jackson.core.json.WriterBasedJsonGenerator
import com.fasterxml.jackson.core.util.BufferRecycler
import com.fasterxml.jackson.databind.ObjectMapper
import _root_.json.bench.model.Data
import _root_.json.bench.{DataReader, DataWriter}

object HandwrittenBench {
  object HandwrittenScalaDataWriter extends DataWriter {
    override def write(seq: Seq[Data]): String = {
      val builder = new StringBuilder("[")

      val dataIterator = seq.iterator
      if(dataIterator.hasNext) {
        writeData(dataIterator.next(), builder)
      }
      while(dataIterator.hasNext) {
        writeData(dataIterator.next(), builder.append(","))
      }

      builder
        .append("]")
        .toString()
    }

    private def writeData(data: Data, builder: StringBuilder): Unit = {
      builder.append("{\"string\":\"").append(data.string).append("\"")
        .append(",\"int\":")  .append(data.int)
        .append(",\"boolean\":")  .append(data.boolean)
        .append(",\"bigDecimal\":")  .append(data.bigDecimal)
        .append(",\"seqInt\":[")

      val intIter = data.seqInt.iterator
      if(intIter.hasNext) {
        builder.append(intIter.next())
      }
      while(intIter.hasNext) {
        builder.append(",").append(intIter.next())
      }

      builder.append("]\"mapStringInt\":{")

      val mapStringIntIter = data.mapStringInt.iterator
      if(mapStringIntIter.hasNext) {
        val (key, value) = mapStringIntIter.next()
        builder.append('"').append(key).append("\":").append(value)
      }
      while(mapStringIntIter.hasNext) {
        val (key, value) = mapStringIntIter.next()
        builder.append(",\"").append(key).append("\":").append(value)
      }

      builder.append('}')

    }
  }

  object HandwrittenJavaDataWriter extends DataWriter {
    override def write(seq: Seq[Data]): String = {
      val builder = new java.lang.StringBuilder("[")

      val dataIterator = seq.iterator
      if(dataIterator.hasNext) {
        writeData(dataIterator.next(), builder)
      }
      while(dataIterator.hasNext) {
        writeData(dataIterator.next(), builder.append(","))
      }

      builder
        .append("]")
        .toString()
    }

    private def writeData(data: Data, builder: java.lang.StringBuilder): Unit = {
      builder.append("{\"string\":\"").append(data.string).append("\"")
        .append(",\"int\":")  .append(data.int)
        .append(",\"boolean\":")  .append(data.boolean)
        .append(",\"bigDecimal\":")  .append(data.bigDecimal)
        .append(",\"seqInt\":[")

      val intIter = data.seqInt.iterator
      if(intIter.hasNext) {
        builder.append(intIter.next())
      }
      while(intIter.hasNext) {
        builder.append(",").append(intIter.next())
      }

      builder.append("]\"mapStringInt\":{")

      val mapStringIntIter = data.mapStringInt.iterator
      if(mapStringIntIter.hasNext) {
        val (key, value) = mapStringIntIter.next()
        builder.append('"').append(key).append("\":").append(value)
      }
      while(mapStringIntIter.hasNext) {
        val (key, value) = mapStringIntIter.next()
        builder.append(",\"").append(key).append("\":").append(value)
      }

      builder.append('}')

    }
  }

  object HandwrittenJacksonDataProcessor extends DataWriter with DataReader {
    private val jsonFactory = {
      val f = new JsonFactory()
      f.configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
      f
    }

    private def jsonGenerator: (JsonGenerator, SegmentedStringWriter) = {
      val bufferRecycler = new BufferRecycler()
      val flags = JsonGenerator.Feature.collectDefaults
      val emptyMapper = new ObjectMapper()
      val writer = new SegmentedStringWriter(bufferRecycler)
      val ioctx = new IOContext(bufferRecycler, writer, false)
      new WriterBasedJsonGenerator(ioctx, flags, emptyMapper, writer) -> writer
    }

    private def jsonParser(s: String): JsonParser = {
      jsonFactory.createParser(s)
    }

    override def write(seq: Seq[Data]): String = {
      val (generator, writer) = jsonGenerator
      generator.writeStartArray()

      val dataIterator = seq.iterator
      while(dataIterator.hasNext) {
        writeData(dataIterator.next(), generator)
      }

      generator.writeEndArray()
      generator.flush()
      generator.close()
      writer.getAndClear()
    }

    private def writeData(data: Data, generator: JsonGenerator): Unit = {
      generator.writeStartObject()
      generator.writeFieldName("string")
      generator.writeString(data.string)

      generator.writeFieldName("int")
      generator.writeNumber(data.int)

      generator.writeFieldName("boolean")
      generator.writeBoolean(data.boolean)

      generator.writeFieldName("bigDecimal")
      generator.writeNumber(data.bigDecimal.bigDecimal)


      generator.writeFieldName("seqInt")
      generator.writeStartArray()
      val intIter = data.seqInt.iterator
      while(intIter.hasNext) {
        generator.writeNumber(intIter.next)
      }
      generator.writeEndArray()

      generator.writeFieldName("mapStringInt")
      generator.writeStartObject()

      val mapStringIntIter = data.mapStringInt.iterator
      while(mapStringIntIter.hasNext) {
        val (key, value) = mapStringIntIter.next()
        generator.writeFieldName(key)
        generator.writeNumber(value)
      }
      generator.writeEndObject()

      generator.writeEndObject()
    }

    override def read(json: String): Seq[Data] = {
      val parser = jsonParser(json)
      val builder = Seq.newBuilder[Data]
      require(parser.nextToken() == JsonToken.START_ARRAY)
      while(parser.nextToken() != JsonToken.END_ARRAY) {
        builder += readDataObject(parser)
      }
      builder.result()
    }

    private def readDataObject(parser: JsonParser): Data = {
      var stringField: String = null
      var stringFieldInitialized: Boolean = false

      var intField: Int = 0
      var intFieldInitialized: Boolean = false

      var booleanField: Boolean = false
      var booleanFieldInitialized: Boolean = false

      var bigDecimalField: BigDecimal = null
      var bigDecimalFieldInitialized: Boolean = false

      var seqIntField: Seq[Int] = null
      var seqIntFieldInitialized: Boolean = false

      var mapStringIntField: Map[String, Int] = null
      var mapStringIntFieldInitialized: Boolean = false

      while(parser.nextToken() != JsonToken.END_OBJECT) {
        val field = parser.getCurrentName

        if(field == "string") {
          stringField = parser.nextTextValue()
          stringFieldInitialized = true
        } else if(field == "int") {
          parser.nextToken()
          intField = parser.getIntValue
          intFieldInitialized = true
        } else if(field == "boolean") {
          booleanField = parser.nextBooleanValue()
          booleanFieldInitialized = true
        } else if(field == "bigDecimal") {
          parser.nextToken()
          bigDecimalField = BigDecimal(parser.getNumberValue.doubleValue())
          bigDecimalFieldInitialized = true
        } else if(field == "seqInt") {
          seqIntField = readSeqOfInt(parser)
          seqIntFieldInitialized = true
        } else {
          mapStringIntField = readMap(parser)
          mapStringIntFieldInitialized = true
        }
      }

      require(stringFieldInitialized && intFieldInitialized && booleanFieldInitialized && bigDecimalFieldInitialized && mapStringIntFieldInitialized)

      Data(
        string = stringField,
        int = intField,
        boolean = booleanField,
        bigDecimal = bigDecimalField,
        seqInt = seqIntField,
        mapStringInt = mapStringIntField
      )
    }

    private def readSeqOfInt(parser: JsonParser): Seq[Int] = {
      require(parser.nextToken() == JsonToken.START_ARRAY)
      val builder = Seq.newBuilder[Int]
      while (parser.nextToken() != JsonToken.END_ARRAY) {
        require(parser.currentToken().id() != JsonTokenId.ID_NO_TOKEN)
        builder += parser.getIntValue
      }
      builder.result()
    }

    private def readMap(parser: JsonParser): Map[String, Int] = {
      require(parser.nextToken() == JsonToken.START_OBJECT)
      val builder = Map.newBuilder[String, Int]
      while (parser.nextToken() != JsonToken.END_OBJECT) {
        require(parser.currentToken().id() == JsonTokenId.ID_FIELD_NAME)
        val name = parser.getCurrentName()
        parser.nextToken()
        val value = parser.getValueAsInt
        builder += name -> value
      }
      builder.result()
    }
  }
}
