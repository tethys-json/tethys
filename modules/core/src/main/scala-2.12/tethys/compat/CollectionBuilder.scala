package tethys.compat

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

trait CollectionBuilder[A, C] {
  def newBuilder: mutable.Builder[A, C]
}

object CollectionBuilder {
  implicit def seqCBFBuilder[A, C[_]](implicit
      cbf: CanBuildFrom[Nothing, A, C[A]]
  ): CollectionBuilder[A, C[A]] = new CollectionBuilder[A, C[A]] {
    override def newBuilder: mutable.Builder[A, C[A]] = cbf()
  }

  implicit def mapCBFBuilder[K, V, M[_, _]](implicit
      cbf: CanBuildFrom[Nothing, (K, V), M[K, V]]
  ): CollectionBuilder[(K, V), M[K, V]] =
    new CollectionBuilder[(K, V), M[K, V]] {
      override def newBuilder: mutable.Builder[(K, V), M[K, V]] = cbf()
    }
}
