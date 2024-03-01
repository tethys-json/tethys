package tethys.compat

import scala.collection.{
  IterableFactory,
  IterableFactoryDefaults,
  MapFactory,
  mutable
}
import scala.language.experimental.macros
import scala.language.higherKinds
import scala.reflect.macros.blackbox

trait CollectionBuilder[A, C] {
  def newBuilder: mutable.Builder[A, C]
}

object CollectionBuilder {

  final class IterableFactoryCollectionBuilder[A, C[_]](
      factory: IterableFactory[C]
  ) extends CollectionBuilder[A, C[A]] {
    override def newBuilder: mutable.Builder[A, C[A]] = factory.newBuilder[A]
  }

  final class MapFactoryCollectionBuilder[K, V, M[_, _]](factory: MapFactory[M])
      extends CollectionBuilder[(K, V), M[K, V]] {
    override def newBuilder: mutable.Builder[(K, V), M[K, V]] =
      factory.newBuilder[K, V]
  }

  implicit def iterableFactoryCollectionBuilder[A, C[
      X
  ] <: IterableFactoryDefaults[X, C]]
      : IterableFactoryCollectionBuilder[A, C] = macro
    CollectionBuilderMacroImpl.fromIterableFactory[A, C]
  implicit def mapFactoryCollectionBuilder[K, V, M[X, Y] <: Map[X, Y]]
      : MapFactoryCollectionBuilder[K, V, M] = macro
    CollectionBuilderMacroImpl.fromMapFactory[K, V, M]

  private class CollectionBuilderMacroImpl(val c: blackbox.Context) {
    import c.universe._

    def fromIterableFactory[A, C[X] <: IterableFactoryDefaults[X, C]](implicit
        A: WeakTypeTag[A],
        C: WeakTypeTag[C[A]]
    ): Tree = {
      val ref = C.tpe.typeSymbol.companion
      q"new tethys.compat.CollectionBuilder.IterableFactoryCollectionBuilder[${A.tpe}, ${C.tpe}]($ref)"
    }

    def fromMapFactory[K, V, M[X, Y] <: Map[X, Y]](implicit
        K: WeakTypeTag[K],
        V: WeakTypeTag[V],
        M: WeakTypeTag[M[K, V]]
    ): Tree = {
      val ref = M.tpe.typeSymbol.companion
      q"new tethys.compat.CollectionBuilder.MapFactoryCollectionBuilder[${K.tpe}, ${V.tpe}, ${M.tpe}]($ref)"
    }
  }

}
