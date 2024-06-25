package tethys.derivation

sealed trait ADTWithType[A]

object ADTWithType {
  case class ADTWithTypeA[A](a: A) extends ADTWithType[A]
  case class ADTWithTypeB[A](a: A, b: ADTWithType[A]) extends ADTWithType[A]
}