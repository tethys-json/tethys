package tethys.writers.instances

import tethys.JsonWriter
import tethys.writers.tokens.TokenWriter

private[tethys] trait EitherWriters {
  implicit def eitherWriter[L, R](implicit
      L: JsonWriter[L],
      R: JsonWriter[R]
  ): JsonWriter[Either[L, R]] = new JsonWriter[Either[L, R]] {
    override def write(
        name: String,
        value: Either[L, R],
        tokenWriter: TokenWriter
    ): Unit = {
      value match {
        case Left(left)   => L.write(name, left, tokenWriter)
        case Right(right) => R.write(name, right, tokenWriter)
      }
    }

    def write(value: Either[L, R], tokenWriter: TokenWriter): Unit = {
      value match {
        case Left(left)   => L.write(left, tokenWriter)
        case Right(right) => R.write(right, tokenWriter)
      }
    }
  }
}
