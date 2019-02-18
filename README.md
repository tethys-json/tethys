# tethys
tethys is a JSON parsing/writing library for Scala

# benchmarks

[see here](./modules/benchmarks)

# Quick start

Add dependencies to your `build.sbt`  

```scala
val tethysVersion = "0.8.0"
libraryDependecies ++= Seq(
  "com.tethys-json" %% "tethys-core" % tethysVersion,
  "com.tethys-json" %% "tethys-jackson" % tethysVersion,
  "com.tethys-json" %% "tethys-derivation" % tethysVersion
)
```

or just

```scala
libraryDependecies ++= Seq(
  "com.tethys-json" %% "tethys" % "0.8.0"
)
```

Also tethys has following integrations:
#### Json4s
[see project page](https://github.com/json4s/json4s)
```scala
libraryDependencies += "com.tethys-json" %% "tethys-json4s" % tethysVersion
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

New writers could be created with object builder or with combination of few writers

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

or just using other JsonWriter

```scala
import tethys._

case class Foo(bar: Int)

JsonWriter.stringWriter.contramap[Foo](_.bar.toString)
```

## JsonReader

JsonReader converts json token from `TokenIterator` to it value
```scala
import tethys._
import tethys.jackson._

"[1, 2, 3, 4]".jsonAs[List[Int]]
```

New readers could be created with builder

```scala
import tethys._
import tethys.jackson._

case class Foo(bar: Int)

implicit val fooReader: JsonReader[Foo] = JsonReader.builder
    .addField[Int]("bar")
    .buildReader(Foo.apply)
    
"""{"bar":1}""".jsonAs[Foo]
```

Also you can select existing reader that depends on other json fields

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
In common case you should prefer semiauto derivation because it's more precise, faster in compile and flexible.

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

In complex cases you could provide some additional information to `jsonWriter` and `jsonReader` functions

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
      // this partial function will be replaced with match in final writer
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
  describe {
    //Any functions are allowed in lambdas 
    ReaderBuilder[Foo]
      .extractReader(_.c).from(_.a)('otherField.as[String]) { // provide reader for Any field
        case (1, "str") => JsonReader[String]
        case (_, "int") => JsonReader[Int]
        case _ => JsonReader[Option[Boolean]]
      }
      .extract(_.a).from(_.b).and("otherField2".as[Int])((b, other) => d.toInt + other) // calculate field that depends on other fields
      .extract(_.e).as[Option[Double]](_.getOrElse(1.0)) // extract field from value of specific type
  }
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

# json4s AST support

In some cases, you may need to work with raw AST,
so tethys can offer you json4s AST support:
```scala
import tethys._
import tethys.jackson._
import tethys.derivation.semiauto._
import tethys.json4s._

import org.json4s.JsonAST._

case class Foo(bar: Int, baz: JValue)

val json = """{"bar": 1, "baz": ["some", {"arbitrary": "json"}]"""
val foo = json.jsonAs[Foo].fold(throw _, identity)

foo.bar // 1
foo.baz // JArray(List(JString("some), JObject("arbitrary" -> JString("json"))))
```