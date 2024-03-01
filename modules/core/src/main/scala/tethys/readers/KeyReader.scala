package tethys.readers

trait KeyReader[A] {
  def read(s: String)(implicit fieldName: FieldName): A
}

object KeyReader {
  implicit lazy val stringKeyReader: KeyReader[String] = new KeyReader[String] {
    override def read(s: String)(implicit fieldName: FieldName): String = s
  }

  implicit lazy val uuidKeyReader: KeyReader[java.util.UUID] =
    new KeyReader[java.util.UUID] {
      override def read(s: String)(implicit
          fieldName: FieldName
      ): java.util.UUID = java.util.UUID.fromString(s)
    }

  implicit lazy val intKeyReader: KeyReader[Int] = new KeyReader[Int] {
    override def read(s: String)(implicit fieldName: FieldName): Int = s.toInt
  }

  implicit lazy val longKeyReader: KeyReader[Long] = new KeyReader[Long] {
    override def read(s: String)(implicit fieldName: FieldName): Long = s.toLong
  }
}
