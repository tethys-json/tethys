package tethys.derivation

private[tethys]
object EnumCompanion:
  inline def getByName[T](name: String): T =
    ${ EnumCompanionMacro.getByName[T]('{ name }) }

  inline def getByOrdinal[T](ordinal: Int): T =
    ${ EnumCompanionMacro.getByOrdinal[T]('{ ordinal }) }
    
  inline def isEnum[T]: Boolean =
    ${ EnumCompanionMacro.isEnum[T] }


private[derivation]
object EnumCompanionMacro:
  import scala.quoted.*
  def getByName[T: scala.quoted.Type](name: Expr[String])(using quotes: Quotes): Expr[T] =
    import quotes.reflect.*
    Select.unique(Ref(TypeRepr.of[T].typeSymbol.companionModule), "valueOf")
      .appliedToArgs(List(name.asTerm))
      .asExprOf[T]


  def getByOrdinal[T: scala.quoted.Type](ordinal: Expr[Int])(using quotes: Quotes): Expr[T] =
    import quotes.reflect.*
    Select.unique(Ref(TypeRepr.of[T].typeSymbol.companionModule), "fromOrdinal")
      .appliedToArgs(List(ordinal.asTerm))
      .asExprOf[T]
    
  def isEnum[T: scala.quoted.Type](using quotes: Quotes): Expr[Boolean] =
    import quotes.reflect.*
    Expr(TypeRepr.of[T].typeSymbol.flags.is(Flags.Enum))


