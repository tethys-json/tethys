package tethys.readers

trait KeyReader[A] {
  def read(s: String)(implicit fieldName: FieldName): A
}

object KeyReader {
  implicit lazy val stringKeyReader: KeyReader[String] = new KeyReader[String] {
    override def read(s: String)(implicit fieldName: FieldName): String = s
  }
}
