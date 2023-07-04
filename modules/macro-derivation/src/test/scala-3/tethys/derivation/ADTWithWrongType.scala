package tethys.derivation

sealed trait ADTWithWrongType[A]

object ADTWithWrongType {

  case class ADTWithWrongTypeA[A](a: A) extends ADTWithWrongType[A]

  case class ADTWithWrongTypeB[A, B](a: A, b: ADTWithWrongType[B]) extends ADTWithWrongType[A]

}