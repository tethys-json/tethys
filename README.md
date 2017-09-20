# tethys
tethys is a JSON parsing/writing library for Scala

# benchmarks

[see here](https://github.com/tethys-json/tethys/tree/master/modules/benchmarks)

# Quick start

Add dependencies to your `build.sbt`  
(waiting groupId on [maven central](https://issues.sonatype.org/browse/OSSRH-34678))

```scala
val tethysVersion = "0.6.0"
libraryDependecies ++= Seq(
  "com.tethys-json" %% "tethys-core" % tethysVersion,
  "com.tethys-json" %% "tethys-jackson" % tethysVersion,
  "com.tethys-json" %% "tethys-derivation" % tethysVersion
)
```

or just

```scala
libraryDependecies ++= Seq(
  "com.tethys-json" %% "tethys" % "0.6.0"
)
```

# core

core module contains all type classes for parsing/writing JSON.
JSON string parsing/writing and derivation are separated to `tethys-jackson` and `tethys-derivation`

## JsonWriter

JsonWriter writes json tokens to `TokenWriter`

```scala
import tethys._

List(1, 2, 3, 4).asJson

//or writer directly to TokenWriter

val tokenWriter = YourWriter

tokenWriter.writeJson(List(1, 2, 3, 4))
```

New writers could be created with object builder and combine them

```scala
import tethys._
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

"[1, 2, 3, 4]".jsonAs[List[Int]]
```

New readers could be created with builder

```scala
import tethys._

case class Foo(bar: Int)

implicit val fooReader: JsonReader[Foo] = JsonReader.builder
    .addField[Int]("bar")
    .buildReader(Foo.apply)
    
"""{"bar":1}""".jsonAs[Foo]
```

Also you can select existing reader that depends on other json fields

```scala
import tethys._

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

`tethys-derivation` provides semiauto and auto macro derivation JsonReader and JsonWriter instances

```scala
import tethys._
import tethys.derivation.auto._
import tethys.derivation.semiauto._

case class Foo(bar: Bar)
case class Bar(seq: Seq[Int])

implicit val barWriter: JsonObjectWriter[Bar] = jsonWriter[Bar] //semiauto
implicit val barReader: JsonReader[Bar] = jsonReader[Bar]

"""{"bar":{"seq":[1,2,3]}}""".jsonAs[Foo] //Foo reader auto derived
``` 

In complex cases you could provide some additional information to `jsonWriter` method

```scala
import tethys._
import tethys.derivation.semiauto._

case class Foo(a: Int, b: String, c: Any)

implicit val fooWriter = jsonWriter[Foo] {
  describe {
    //Any functions are allowed in lambdas 
    WriterBuilder[Foo]()
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