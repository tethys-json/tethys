package tethys.writers.tokens

import tethys.JsonWriter

import java.time.{Duration, LocalDate, LocalDateTime}
import java.util.UUID

object TestModels {

  implicit val uuidWriter: JsonWriter[UUID] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val localDateWriter: JsonWriter[LocalDate] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val localDateTimeWriter: JsonWriter[LocalDateTime] =
    JsonWriter.stringWriter.contramap(_.toString)

  implicit val durationWriter: JsonWriter[Duration] =
    JsonWriter.stringWriter.contramap(_.toString)

  case class Person(name: String, age: Int, email: Option[String])

  object Person {
    implicit val personWriter: JsonWriter[Person] = JsonWriter
      .obj[Person]
      .addField("name")(_.name)
      .addField("age")(_.age)
      .addField("email")(_.email)
  }
  case class Address(
      street: String,
      city: String,
      country: String,
      postalCode: Option[String]
  )

  object Address {
    implicit val addressWriter: JsonWriter[Address] = JsonWriter
      .obj[Address]
      .addField("street")(_.street)
      .addField("city")(_.city)
      .addField("country")(_.country)
      .addField("postalCode")(_.postalCode)
  }

  case class Employee(
      id: Long,
      person: Person,
      address: Address,
      department: String,
      salary: BigDecimal
  )

  object Employee {
    implicit val employeeWriter: JsonWriter[Employee] = JsonWriter
      .obj[Employee]
      .addField("id")(_.id)
      .addField("person")(_.person)
      .addField("address")(_.address)
      .addField("department")(_.department)
      .addField("salary")(_.salary)
  }

  case class Department(
      name: String,
      employees: List[Employee],
      tags: Set[String],
      metadata: Map[String, String]
  )

  object Department {
    implicit val departmentWriter: JsonWriter[Department] = JsonWriter
      .obj[Department]
      .addField("name")(_.name)
      .addField("employees")(_.employees)
      .addField("tags")(_.tags)
      .addField("metadata")(_.metadata)
  }

  case class Container[T](
      id: String,
      data: T,
      timestamp: Long
  )

  object Container {
    implicit def containerWriter[T: JsonWriter]: JsonWriter[Container[T]] =
      JsonWriter
        .obj[Container[T]]
        .addField("id")(_.id)
        .addField("data")(_.data)
        .addField("timestamp")(_.timestamp)
  }

  case class TreeNode(
      value: String,
      children: List[TreeNode]
  )

  case class CustomTypes(
      uuid: java.util.UUID,
      date: java.time.LocalDate,
      datetime: java.time.LocalDateTime,
      bigInt: BigInt,
      bigDecimal: BigDecimal,
      duration: java.time.Duration
  )

  object CustomTypes {
    implicit val customTypesWriter: JsonWriter[CustomTypes] = JsonWriter
      .obj[CustomTypes]
      .addField("uuid")(_.uuid)
      .addField("date")(_.date)
      .addField("datetime")(_.datetime)
      .addField("bigInt")(_.bigInt)
      .addField("bigDecimal")(_.bigDecimal)
      .addField("duration")(_.duration)
  }
}
