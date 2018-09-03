lazy val scalaTestVersion = "3.0.5"

lazy val commonSettings = Seq(
  version := "0.7.0",
  organization := "com.tethys-json",
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.6", "2.13.0-M2"),

  licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),
  homepage := Some(url("https://github.com/tethys-json/tethys")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/tethys-json/tethys"),
      "scm:git@github.com:tethys-json/tethys.git"
    )
  ),
  developers := List(
    Developer(
      id = "eld0727",
      name = "Aleksei Otts",
      email = "eld0727@gmail.com",
      url = url("https://github.com/eld0727")
    )
  ),
  publishMavenStyle := true,
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishArtifact in Test := false
)

lazy val tethys = project.in(file("."))
  .settings(commonSettings)
  .dependsOn(core, `macro-derivation`, `jackson-backend`)
  .aggregate(core, `macro-derivation`, `jackson-backend`, json4s)

lazy val core = project.in(file("./modules/core"))
  .settings(commonSettings)
  .settings(
    name := "tethys-core",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    )
  )

lazy val `macro-derivation` = project.in(file("./modules/macro-derivation"))
  .settings(commonSettings)
  .settings(
    name := "tethys-derivation",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided",

      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    )
  ).dependsOn(core)

lazy val `jackson-backend` = project.in(file("./modules/jackson-backend"))
  .settings(commonSettings)
  .settings(
    name := "tethys-jackson",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.9.1",

      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    )
  ).dependsOn(core)

lazy val json4s = project.in(file("./modules/json4s"))
  .settings(commonSettings)
  .settings(
    name := "tethys-json4s",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-core" % "3.5.3",

      "org.scalatest" %% "scalatest" % scalaTestVersion % Test
    )
  ).dependsOn(core)

lazy val benchmarks = project.in(file("./modules/benchmarks"))
  .settings(commonSettings)
  .settings(
    scalaVersion := "2.11.11",
    crossScalaVersions := Seq("2.11.11"),
    publishTo := None,
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.3",
      "org.json4s" %% "json4s-native" % "3.5.1",
      "org.json4s" %% "json4s-jackson" % "3.5.1",
      "com.typesafe.play" %% "play-json" % "2.4.11",
      "com.github.fomkin" %% "pushka-json" % "0.8.0",
      "io.circe" %% "circe-core" % "0.9.0-M1",
      "io.circe" %% "circe-generic" % "0.9.0-M1",
      "io.circe" %% "circe-parser" % "0.9.0-M1",

      "org.knowm.xchart" % "xchart" % "3.5.0" exclude("de.erichseifert.vectorgraphics2d", "VectorGraphics2D") withSources()
    ),
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
  .dependsOn(tethys)
  .enablePlugins(JmhPlugin)