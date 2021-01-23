package tethys.readers.instances

private[tethys] trait LiteralReaders {
  implicit def booleanLiteralReader[L <: Boolean](implicit L: ValueOf[L]): JsonReader[L] = ???

  implicit def stringLiteralReader[L <: String](implicit L: ValueOf[L]): JsonReader[L] = ???

  implicit def shortLiteralReader[L <: Short](implicit L: ValueOf[L]): JsonReader[L] = ???

  implicit def intLiteralReader[L <: Int](implicit L: ValueOf[L]): JsonReader[L] = ???

  implicit def longLiteralReader[L <: Long](implicit L: ValueOf[L]): JsonReader[L] = ???

  implicit def floatLiteralReader[L <: Float](implicit L: ValueOf[L]): JsonReader[L] = ???

  implicit def doubleLiteralReader[L <: Double](implicit L: ValueOf[L]): JsonReader[L] = ???
}
