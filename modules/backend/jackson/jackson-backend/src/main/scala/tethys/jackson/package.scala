package tethys

import java.io.Reader
import com.fasterxml.jackson.core.{
  JsonFactory,
  JsonFactoryBuilder,
  JsonGenerator
}
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.{TokenIterator, TokenIteratorProducer}
import tethys.writers.tokens.{
  TokenWriter,
  TokenWriterConfig,
  TokenWriterProducer
}

package object jackson {
  lazy val defaultJsonFactory: JsonFactory =
    new JsonFactoryBuilder()
      .configure(JsonFactory.Feature.INTERN_FIELD_NAMES, false)
      .build()

  class JacksonTokenWriterProducer(
      jsonFactory: JsonFactory,
      // used for compatibility where tethys.jackson.pretty import is required
      modifyConfig: TokenWriterConfig => TokenWriterConfig
  ) extends TokenWriterProducer {

    private def updateGeneratorFromConfig(
        generator: JsonGenerator,
        config: TokenWriterConfig
    ): JsonGenerator = {
      if (config == TokenWriterConfig.default)
        generator
      else {
        val first =
          if (config.indentionStep == 2)
            generator.useDefaultPrettyPrinter()
          else
            generator

        val second =
          if (config.escapeUnicode)
            first.setHighestNonEscapedChar(127)
          else
            first
        second
      }
    }

    override def produce(config: TokenWriterConfig): TokenWriter = {
      val generator = updateGeneratorFromConfig(
        jsonFactory.createGenerator(new java.io.StringWriter()),
        modifyConfig(config)
      )
      new JacksonTokenWriter(generator)
    }
  }

  implicit def jacksonTokenWriterProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): JacksonTokenWriterProducer =
    new JacksonTokenWriterProducer(jsonFactory, identity)

  implicit def jacksonTokenIteratorProducer(implicit
      jsonFactory: JsonFactory = defaultJsonFactory
  ): TokenIteratorProducer = new TokenIteratorProducer {
    override def fromReader(
        reader: Reader
    ): Either[ReaderError, TokenIterator] = {
      ReaderError.catchNonFatal(
        JacksonTokenIterator.fromFreshParser(jsonFactory.createParser(reader))
      )(FieldName())
    }
  }
}
