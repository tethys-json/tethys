val commonSettings = Seq(
  version := "0.6.1-SNAPSHOT",
  organization := "com.tethys-json",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.11.8", "2.12.2"),
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
)

lazy val tethys = project.in(file("."))
  .settings(commonSettings)
  .dependsOn(core, `macro-derivation`, `jackson-backend`)
  .aggregate(core, `macro-derivation`, `jackson-backend`)

lazy val core = project.in(file("./modules/core"))
  .settings(commonSettings)
  .settings(
  name := "tethys-core"
)

lazy val `macro-derivation` = project.in(file("./modules/macro-derivation"))
  .settings(commonSettings)
  .settings(
    name := "tethys-macro-derivation",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
    )
  ).dependsOn(core)

lazy val `jackson-backend` = project.in(file("./modules/jackson-backend"))
  .settings(commonSettings)
  .settings(
    name := "tethys-jackson-backend",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.9.1"
    )
  ).dependsOn(core)

lazy val benchmarks = project.in(file("./modules/benchmarks"))
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.3",
      "org.json4s" %% "json4s-native" % "3.5.1",
      "org.json4s" %% "json4s-jackson" % "3.5.1",
      "com.typesafe.play" %% "play-json" % "2.4.11",
      "com.github.fomkin" %% "pushka-json" % "0.8.0",
      "io.circe" %% "circe-core" % "0.9.0-M1",
      "io.circe" %% "circe-generic" % "0.9.0-M1",
      "io.circe" %% "circe-parser" % "0.9.0-M1"
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .dependsOn(tethys)
  .enablePlugins(JmhPlugin)