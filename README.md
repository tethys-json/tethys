| CI | Release | 
| --- | --- |
| [![Build Status](https://github.com/tethys-json/tethys/workflows/Scala%20CI/badge.svg)](https://github.com/tethys-json/tethys/actions) | [![Maven Central](https://img.shields.io/maven-central/v/com.tethys-json/tethys-core_2.13.svg)](https://search.maven.org/search?q=com.tethys-json.tethys-json) | 

# tethys
tethys is AST free json library for Scala

It's advantages:
1. Performant
    * Built in bridge to jackson [(benchmarks)](./modules/benchmarks)
   

2. User friendly
    * Build reader/writer by hand for product and sum types
    * Configurable recursive semiauto derivation
    * Discriminator support for sum types derivation

Read more about library usage bellow:
- [Scala 3](https://github.com/tethys-json/tethys?tab=readme-ov-file#scala-3)
- [Scala 2](https://github.com/tethys-json/tethys?tab=readme-ov-file#scala-2)


# Scala 3

## Quick start

```scala
val tethysVersion = "latest version in badge"
libraryDependencies ++= Seq(
  "com.tethys-json" %% "tethys-core" % tethysVersion,
  "com.tethys-json" %% "tethys-jackson" % tethysVersion
)
```

## Read/Write JSON API
tethys provides extension methods allowing you to read and write JSON

They look something like this:
```scala 3
package tethys

extension [A](value: A)
  def asJson(using 
    jw: JsonWriter[A],
    twp: TokenWriterProducer
  ): String = ???

extension (value: String)
  def readJson[A](using 
    jr: JsonReader[A],
    tip: TokenIteratorProducer
  ): Either[ReaderError, A] = ???
```

Tethys provides **TokenWriterProducer** and **TokenIteratorProducer** automatically,
so in most cases you only need to provide **JsonReader** or **JsonWriter**.
Let's see how can we get one.


## Basic instances
tethys provides **JsonReader** and **JsonWriter** instances for a bunch of basic types

Check links below to see exact ones:

[JsonReader instances](./modules/core/src/main/scala/tethys/readers/instances/AllJsonReaders.scala)

[JsonWriters instances](./modules/core/src/main/scala/tethys/writers/instances/AllJsonWriters.scala)

## Build instances by hand

### map and contramap

You can create new instances for your types using:
1. **contramap** on already existing writer
2. **map** on already existing reader

```scala 3
import tethys.*

case class StringWrapper(value: String) extends AnyVal

given JsonWriter[StringWrapper] =
   JsonWriter[String].contramap(_.value)
   
given JsonReader[StringWrapper] =
   JsonReader[String].map(StringWrapper(_))
```

### JsonWriter

To build JsonWriter for case class you can use `obj` method on its companion object.  

```scala 3
import tethys.*

  case class MobileSession(
    id: Long, 
    deviceId: String, 
    userId: java.lang.UUID
  ) extends Session
  
object MobileSession:
  given JsonObjectWriter[MobileSession] = JsonWriter.obj[MobileSession]
    .addField("id")(_.id)
    .addField("deviceId")(_.deviceId)
    .addField("userId")(_.userId)

```

You can concat multiple **JsonObjectWriter**.  
Combining concatenation with derivation allows to create **JsonWriter** for sealed trait.
To derive JsonWriter for sealed trait you need to have **JsonObjectWriter** instances for all subtypes in scope

```scala 3

given JsonWriter[Session] =
   JsonWriter.obj[Session].addField("typ")(_.typ) ++ JsonObjectWriter.derived[Session]

```


### JsonReader

To build JsonReader for case class you can use `builder` method on its companion object.

```scala 3
import tethys.*

  case class MobileSession(
    id: Long, 
    deviceId: String, 
    userId: java.lang.UUID
  ) extends Session("mobile")
  
  object Mobile:
    given JsonReader[MobileSession] = JsonReader.builder
      .addField[Long]("id")
      .addField[String]("deviceId")
      .addField[java.lang.UUID]("userId")
      .buildReader(MobileSession(_, _, _))
  

```   

To build JsonReader for sealed trait you can use `selectReader` after adding some field:

```scala 3
import tethys.*
  
  object Session:
    given webReader: JsonReader[WebSession] = ???
    given mobileReader: JsonReader[MobileSession] = ???
    
    given JsonReader[Session] = JsonReader.builder
      .addField[String]("typ")
      .selectReader {
         case "web" => webReader
         case "mobile" => mobileReader
      }
  

```  


## Derivation

All examples consider you made this imports:
```scala 3
import tethys.*
import tethys.jackson.* // or tethys.jackson.pretty.* for pretty printing
```


### Basic enums
1. **StringEnumJsonWriter** and **StringEnumJsonReader**

```scala 3
enum SessionType derives StringEnumJsonWriter, StringEnumJsonReader:
  case Mobile, Web
 
case class Session(typ: SessionType) derives JsonReader, JsonObjectWriter

val session = Session(typ = SessionType.Mobile)
val json = """{"typ": "Mobile"}"""

json.jsonAs[Session] == Right(session)
session.asJson == json
```
2. **OrdinalEnumJsonWriter** and **OrdinalEnumJsonReader**

```scala 3
enum SessionType derives OrdinalEnumJsonWriter, OrdinalEnumJsonReader:
  case Mobile, Web
 
case class Session(typ: SessionType) derives JsonReader, JsonObjectWriter

val session = Session(typ = SessionType.Web)
val json = """{"typ": "1"}"""

json.jsonAs[Session] == Right(session)
session.asJson == json
```

### Case classes

```scala 3
case class Session(
    id: Long, 
    userId: String
) derives JsonReader, JsonObjectWriter

val session = Session(id = 123, userId = "3-X56812")
val json = """{"id": 123, "userId": "3-X56812"}"""

json.jsonAs[Session] == Right(session)
session.asJson == json
```

### Sealed traits and enums
To derive **JsonReader** you **must** provide a discriminator.
This can be done via **selector** annotation.    
Discriminator for **JsonWriter** is optional.

If you don't need readers/writers for subtypes, you can omit them,
they will be derived recursively for your trait/enum.

```scala 3
import tethys.selector

sealed trait UserAccount(@selector val typ: String) derives JsonReader, JsonObjectWriter

object UserAccount:
   case class Customer(
        id: Long,
        phone: String
   ) extends UserAccount("Customer")
   
   case class Employee(
        id: Long,
        phone: String,
        position: String
   ) extends UserAccount("Employee")

val account: UserAccount = UserAccount.Customer(id = 123, phone = "+12394283293"
val json = """{"typ": "Customer", "id": 123, "userId": "+12394283293"}"""

json.jsonAs[UserAccount] == Right(account)
account.asJson == json
```

## Configuration

1. You can configure only case class derivation
2. To configure **JsonReader** use **ReaderBuilder**
3. To configure **JsonWriter** use **WriterBuilder**
4. Configuration can be provided:
   * **directly to derived method**
   ```scala 3
      given JsonWriter[UserAccount.Customer] = 
        JsonObjectWriter.derived {
          WriterBuilder[UserAccount.Customer]
        }
   ```
   * **as an inline given to derives**
   ```scala 3
      object Customer:
        inline given WriterBuilder[UserAccount.Customer] =
          WriterBuilder[UserAccount.Customer]
   ```
   P.S. There are empty **WriterBuilder** in the examples to simplify demonstration of two approaches. You shouldn't use empty one
5. **WriterBuilder** features
```scala 3
case class Foo(a: Int, b: String, c: Any, d: Boolean, e: Double)

inline given WriterBuilder[Foo] =
   WriterBuilder[Foo]
     // choose field style
     .fieldStyle(FieldStyle.UpperSnakeCase)
     // remove field
     .remove(_.b)
     // add new field
     .add("d")(_.b.trim)
     // rename field
     .rename(_.e)("z")
     // update field (also you can rename it using withRename after choosing field)
     .update(_.a)(_ + 1)
     // update field from root (same as update, but function is from root element)
     .update(_.d).fromRoot(foo => if (foo.d) foo.a else foo.a / 2)
     // possibility to semiauto derive any
     .update(_.c) {
        case s: String => s
        case i: Int if i % 2 == 0 => i / 2
        case i: Int => i + 1
        case other => other.toString
     }
```
6. **ReaderBuilder** features
```scala 3

inline given ReaderBuilder[Foo] =
  ReaderBuilder[Foo]
    // extract field from a value of a specific type
    .extract(_.e).as[Option[Double]](_.getOrElse(1.0))
  
    // extract field as combination of model fields and some other fields from json
    .extract(_.a).from(_.b).and[Int]("otherField2")((b, other) => d.toInt + other)
  
    // provide reader for Any field
    .extractReader(_.c).from(_.a) {
       case 1 => JsonReader[String]
       case 2 => JsonReader[Int]
       case _ => JsonReader[Option[Boolean]]
    }
```


## integrations
In some cases, you may need to work with raw AST,
so tethys can offer you **circe** and **json4s** AST support

#### Circe
[see project page](https://github.com/circe/circe)
```scala
libraryDependencies += "com.tethys-json" %% "tethys-circe" % tethysVersion
```

```scala 3
import tethys.*
import tethys.jackson.*
import tethys.circe.*

import io.circe.Json

case class Foo(bar: Int, baz: Json) derives JsonReader

val json = """{"bar": 1, "baz": ["some", {"arbitrary": "json"}]}"""
val foo = json.jsonAs[Foo].fold(throw _, identity)

foo.bar // 1: Int
foo.baz // [ "some", { "arbitrary" : "json" } ]: io.circe.Json
```

#### Json4s
[see project page](https://github.com/json4s/json4s)
```scala
libraryDependencies += "com.tethys-json" %% "tethys-json4s" % tethysVersion
```

```scala
import tethys.*
import tethys.jackson.*
import tethys.json4s.*

import org.json4s.JsonAST.*

case class Foo(bar: Int, baz: JValue) derives JsonReader

val json = """{"bar": 1, "baz": ["some", {"arbitrary": "json"}]"""
val foo = json.jsonAs[Foo].fold(throw _, identity)

foo.bar // 1
foo.baz // JArray(List(JString("some"), JObject("arbitrary" -> JString("json"))))
```
#### Enumeratum
[see project page](https://github.com/lloydmeta/enumeratum)
```scala
libraryDependencies += "com.tethys-json" %% "tethys-enumeratum" % tethysVersion
```

enumeratum module provides a bunch of mixins for your Enum classes.
```scala

import enumeratum.{Enum, EnumEntry}
import tethys.enumeratum.*

sealed trait Direction extends EnumEntry
case object Direction extends Enum[Direction] 
  with TethysEnum[Direction] // provides JsonReader and JsonWriter instances 
  with TethysKeyEnum[Direction] { // provides KeyReader and KeyWriter instances
  
  
  case object Up extends    Direction
  case object Down extends  Direction
  case object Left extends  Direction
  case object Right extends Direction

  val values = findValues
}

```

### Migration notes

When migrating to **scala 3** you should use **0.29.0** version.

Scala 3 derivation API in **0.29.0** has a lot of deprecations and is not fully compatible in compile time with **0.28.4**, including:

1. **WriterDescription** and **ReaderDescription** are deprecated along with **describe** macro.
   Use **WriterBuilder** and **ReaderBuilder** directly instead


2. **DependentField** model for **ReaderBuilder** has changed.
   Now `extract field from` feature works like this:
    * exactly one **from** call
    * chain of **and** calls (until compiler lets you)
    * both methods **from/and** has two forms
        * select some field from your model
        * provide type to method and name of field as string parameter

```scala 3
   ReaderBuilder[SimpleType]
     .extract(_.i).from(_.d).and[Double]("e")((d, e) => (d + e).toInt)
```

3. **0.28.4 scala 3 enum support** was changed. [See more](https://github.com/tethys-json/tethys?tab=readme-ov-file#basic-enums)


4. `updatePartial` for **WriterBuilder** is deprecated. Use ```update``` instead

5. all derivation api were moved directly into core module in **tethys** package, including
    * FieldStyle
    * WriterBuilder
    * ReaderBuilder

6. **auto** derivation is deprecated. Use derives on toplevel type instead

# Scala 2

## Quick start
Add dependencies to your `build.sbt`

```scala
val tethysVersion = "latest version in badge"
libraryDependencies ++= Seq(
  "com.tethys-json" %% "tethys-core" % tethysVersion,
  "com.tethys-json" %% "tethys-jackson213" % tethysVersion,
  "com.tethys-json" %% "tethys-derivation" % tethysVersion
)
```

```scala
libraryDependencies ++= Seq(
  "com.tethys-json" %% "tethys" % "latest version in badge"
)
```

# core

core module contains all type classes for parsing/writing JSON.
JSON string parsing/writing and derivation are separated to `tethys-jackson` and `tethys-derivation`

## JsonWriter

JsonWriter writes json tokens to `TokenWriter`

```scala
import tethys._
import tethys.jackson._

List(1, 2, 3, 4).asJson

//or write directly to TokenWriter

val tokenWriter = YourWriter

tokenWriter.writeJson(List(1, 2, 3, 4))
```

New writers can be created with an object builder or with a combination of a few writers

```scala
import tethys._
import tethys.jackson._
import scala.reflect.ClassTag

case class Foo(bar: Int)

def classWriter[A](implicit ct: ClassTag[A]): JsonObjectWriter[A] = {
    JsonWriter.obj[A].addField("clazz")(_ => ct.toString())
}

implicit val fooWriter: JsonObjectWriter[Foo] = {
  classWriter[Foo] ++ JsonWriter.obj[Foo].addField("bar")(_.bar)
}

Foo(1).asJson
```

or just using another JsonWriter

```scala
import tethys._

case class Foo(bar: Int)

JsonWriter.stringWriter.contramap[Foo](_.bar.toString)
```

## JsonReader

JsonReader converts a json token from `TokenIterator` to its value
```scala
import tethys._
import tethys.jackson._

"[1, 2, 3, 4]".jsonAs[List[Int]]
```

New readers can be created with a builder

```scala
import tethys._
import tethys.jackson._

case class Foo(bar: Int)

implicit val fooReader: JsonReader[Foo] = JsonReader.builder
    .addField[Int]("bar")
    .buildReader(Foo.apply)
    
"""{"bar":1}""".jsonAs[Foo]
```

Also you can select an existing reader that depends on other json fields

```scala
import tethys._
import tethys.jackson._

trait FooBar
case class Foo(foo: Int) extends FooBar
case class Bar(bar: String)  extends FooBar

val fooReader: JsonReader[Foo] = JsonReader.builder
    .addField[Int]("foo")
    .buildReader(Foo.apply)
    
val barReader: JsonReader[Bar] = JsonReader.builder
    .addField[String]("bar")
    .buildReader(Bar.apply)
    
implicit val fooBarReader: JsonReader[FooBar] = JsonReader.builder
    .addField[String]("clazz")
    .selectReader[FooBar] {
      case "Foo" => fooReader
      case _ => barReader 
    }    
    
"""{"clazz":"Foo","foo":1}""".jsonAs[FooBar]
```

Please check out `tethys` package object for all available syntax Ops classes

# derivation

`tethys-derivation` provides semiauto and auto macro derivation JsonReader and JsonWriter instances.  
In most cases you should prefer semiauto derivation because it's more precise, faster in compilation and flexible.

```scala
import tethys._
import tethys.jackson._
import tethys.derivation.auto._
import tethys.derivation.semiauto._

case class Foo(bar: Bar)
case class Bar(seq: Seq[Int])

implicit val barWriter: JsonObjectWriter[Bar] = jsonWriter[Bar] //semiauto
implicit val barReader: JsonReader[Bar] = jsonReader[Bar]

"""{"bar":{"seq":[1,2,3]}}""".jsonAs[Foo] //Foo reader auto derived
``` 

In complex cases you can provide some additional information to `jsonWriter` and `jsonReader` functions

```scala
import tethys._
import tethys.derivation.builder._
import tethys.derivation.semiauto._

case class Foo(a: Int, b: String, c: Any, d: Boolean, e: Double)

implicit val fooWriter = jsonWriter[Foo] {
  describe {
    //Any functions are allowed in lambdas
    WriterBuilder[Foo]
      .remove(_.b)
      .add("d")(_.b.trim)
      .update(_.a)(_ + 1)
      // the only way to semiauto derive Any
      // this partial function will be replaced with match in the final writer
      .updatePartial(_.c) {  
        case s: String => s
        case i: Int if i % 2 == 0 => i / 2
        case i: Int => i + 1
        case other => other.toString 
      }
      .update(_.d).fromRoot(foo => if(foo.d) foo.a else foo.a / 2) //same as update but function accepts root element
      .updatePartial(_.e).fromRoot { //same as updatePartial but function accepts root element
        case Foo(1, _, _, _, e) => e
        case Foo(2, _, _, _, e) => e % 2
        case foo => e.toString
      }
  }
}

implicit val fooReader = jsonReader[Foo] {
    //Any functions are allowed in lambdas
    ReaderBuilder[Foo]
      .extractReader(_.c).from(_.a)('otherField.as[String]) { // provide reader for Any field
        case (1, "str") => JsonReader[String]
        case (_, "int") => JsonReader[Int]
        case _ => JsonReader[Option[Boolean]]
      }
      .extract(_.a).from(_.b).and("otherField2".as[Int])((b, other) => d.toInt + other) // calculate a field that depends on other fields
      .extract(_.e).as[Option[Double]](_.getOrElse(1.0)) // extract a field from a value of a specific type
}
```


# jackson

`tethys-jackson` module provides bridge instances for jackson streaming api

```scala
import tethys.jackson._
//import tethys.jackson.pretty._ //pretty writing

//that's it. welcome to use jackson
```

# complex case
```scala
import tethys._
import tethys.jackson._
import tethys.derivation.auto._

case class Foo(bar: Bar)
case class Bar(seq: Seq[Int])

val foo = """{"bar":{"seq":[1,2,3]}}""".jsonAs[Foo].fold(throw _, identity)
val json = foo.asJson
```
