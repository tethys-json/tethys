package tethys

import scala.deriving.Mirror

import tethys.{JsonObjectWriter, JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import scala.annotation.experimental

package object derivation {
  extension (underlying: JsonReader.type) {
    @experimental
    inline def derived[T](using Mirror.Of[T]): JsonReader[T] = jsonReader[T]
  }

  extension (underlying: JsonWriter.type) {
    @experimental
    inline def derived[T](using Mirror.Of[T]): JsonWriter[T] = jsonWriter[T]
  }

  extension (underlying: JsonObjectWriter.type) {
    @experimental
    inline def derived[T](using Mirror.Of[T]): JsonObjectWriter[T] = jsonWriter[T]
  }
}
