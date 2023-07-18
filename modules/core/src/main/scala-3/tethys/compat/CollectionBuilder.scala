package tethys.compat

import scala.collection.{IterableFactory, MapFactory, mutable}
import scala.quoted.*

trait CollectionBuilder[A, C] {
  def newBuilder: mutable.Builder[A, C]
}

object CollectionBuilder {
  final class IterableFactoryCollectionBuilder[A, C[_]](factory: IterableFactory[C]) extends CollectionBuilder[A, C[A]] {
    def newBuilder: mutable.Builder[A, C[A]] = factory.newBuilder[A]
  }

  final class MapFactoryCollectionBuilder[K, V, M[_, _]](factory: MapFactory[M]) extends CollectionBuilder[(K, V), M[K, V]] {
    def newBuilder: mutable.Builder[(K, V), M[K, V]] = factory.newBuilder[K, V]
  }

  inline given iterableFactoryCollectionBuilder[A, C[X] <: Iterable[X]]: CollectionBuilder[A, C[A]] =
    ${CollectionBuilderMacroImpl.fromIterableFactory[A, C]}

  inline given mapFactoryCollectionBuilder[K, V, M[X, Y] <: Map[X, Y]]: MapFactoryCollectionBuilder[K, V, M] =
    ${CollectionBuilderMacroImpl.fromMapFactory[K, V, M]}

  object CollectionBuilderMacroImpl {
    def fromIterableFactory[A: Type, C[X] <: Iterable[X]: Type](using Quotes): Expr[IterableFactoryCollectionBuilder[A, C]] = {
      import quotes.reflect.*

      val factory = Ref(TypeRepr.of[C].typeSymbol.companionModule).asExprOf[IterableFactory[C]]
      '{new tethys.compat.CollectionBuilder.IterableFactoryCollectionBuilder[A, C]($factory)}
    }

    def fromMapFactory[K: Type, V: Type, M[X, Y] <: Map[X, Y]: Type](using Quotes): Expr[MapFactoryCollectionBuilder[K, V, M]] = {
      import quotes.reflect.*

      val factory = Ref(TypeRepr.of[M].typeSymbol.companionModule).asExprOf[MapFactory[M]]
      '{new tethys.compat.CollectionBuilder.MapFactoryCollectionBuilder[K, V, M]($factory)}
    }
  }
}