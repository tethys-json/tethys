package tethys.readers

final case class FieldName(value: () => String) extends AnyVal {
  self =>

  def appendFieldName(s: String): FieldName = FieldName(() => s"${value()}.$s")

  def appendArrayIndex(i: Int): FieldName = FieldName(() => s"${value()}[$i]")
}
object FieldName {
  val Root: FieldName = new FieldName(() => "[ROOT]")

  def apply(): FieldName = Root

  def apply(value: String): FieldName = new FieldName(() => value)
}
