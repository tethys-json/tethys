package tethys.core.readers

final case class FieldName(value: () => String) {
  self =>

  def appendFieldName(s: String): FieldName = FieldName(() => s"${value()}.s")

  def appendArrayIndex(i: Int): FieldName = FieldName(() => s"${value()}[$i]")
}
object FieldName {
  def apply(): FieldName = new FieldName(() => "[ROOT]")

  def apply(value: String): FieldName = new FieldName(() => value)
}
