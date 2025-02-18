ThisBuild / scalaVersion := "3.3.4"

lazy val commonSettings = Seq(
  organization := "com.tethys-json",
  licenses := Seq(
    "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
  ),
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
    ),
    Developer(
      id = "REDNBLACK",
      name = "Boris Potepun",
      email = "boris.p@protonmail.com",
      url = url("https://github.com/REDNBLACK")
    ),
    Developer(
      id = "MrIrre",
      name = "Erlan Zhaygutov",
      email = "zhaigutov.erlan@gmail.com",
      url = url("https://github.com/MrIrre")
    ),
    Developer(
      id = "goshacodes",
      name = "Georgii Kovalev",
      email = "goshacodes@gmail.com",
      url = url("https://github.com/goshacodes")
    )
  ),
  Test / publishArtifact := false
)

lazy val testSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest-flatspec" % "3.2.19" % Test,
    "org.scalatest" %% "scalatest-shouldmatchers" % "3.2.19" % Test
  )
)

lazy val tethys = project
  .in(file("."))
  .settings(
    publishTo := None,
    crossScalaVersions := Seq.empty,
    commonSettings
  )
  .aggregate(
    core,
    `jackson-211`,
    `jackson-212`,
    `jackson-213`,
    json4s,
    circe,
    refined,
    enumeratum,
    cats
  )

lazy val modules = file("modules")

lazy val core = project
  .in(modules / "core")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-core"
  )

lazy val jackson = modules / "backend" / "jackson"

lazy val jacksonSettings = Seq(
  Compile / unmanagedSourceDirectories += jackson / "jackson-backend" / "src" / "main",
  Test / unmanagedSourceDirectories += jackson / "jackson-backend" / "src" / "test",
  Test / unmanagedResourceDirectories += jackson / "jackson-backend" / "src" / "test" / "resources"
)

lazy val `jackson-211` = project
  .in(jackson / "jackson-211")
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-jackson211",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.11.4"
    )
  )
  .dependsOn(core)

lazy val `jackson-212` = project
  .in(jackson / "jackson-212")
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-jackson212",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.12.7"
    )
  )
  .dependsOn(core)

lazy val `jackson-213` = project
  .in(jackson / "jackson-213")
  .settings(commonSettings)
  .settings(jacksonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-jackson213",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.13.5"
    )
  )
  .dependsOn(core)

lazy val ast = modules / "ast"

lazy val circe = project
  .in(ast / "circe")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.10"
    )
  )
  .dependsOn(core, `jackson-213` % "compile->test;test->test")

lazy val json4s = project
  .in(ast / "json4s")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-json4s",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-ast" % "4.0.7"
    )
  )
  .dependsOn(core)

lazy val integrations = modules / "integrations"

lazy val cats = project
  .in(integrations / "cats")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.12.0"
    )
  )
  .dependsOn(core)

lazy val enumeratum = project
  .in(integrations / "enumeratum")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-enumeratum",
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.7.5"
    )
  )
  .dependsOn(core)

lazy val refined = project
  .in(integrations / "refined")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys1-refined",
    libraryDependencies ++= Seq(
      "eu.timepit" %% "refined" % "0.10.3"
    )
  )
  .dependsOn(core)

lazy val benchmarks = project
  .in(modules / "benchmarks")
  .settings(commonSettings)
  .settings(
    publishTo := None,
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.6",
      "org.json4s" %% "json4s-native" % "4.0.7",
      "org.json4s" %% "json4s-jackson" % "4.0.7",
      "io.circe" %% "circe-core" % "0.14.9",
      "io.circe" %% "circe-generic" % "0.14.9",
      "io.circe" %% "circe-jawn" % "0.14.9",
      "io.circe" %% "circe-jackson210" % "0.14.0",
      "dev.zio" %% "zio-json" % "0.7.1",
      "com.typesafe.play" %% "play-json" % "2.10.5",
      "org.knowm.xchart" % "xchart" % "3.8.2" exclude ("de.erichseifert.vectorgraphics2d", "VectorGraphics2D") withSources ()
    ),
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-Ymacro-annotations")
        case _             => Seq.empty
      }
    }
  )
  .dependsOn(core, `jackson-213`)
  .enablePlugins(JmhPlugin)
