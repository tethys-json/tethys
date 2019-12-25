lazy val commonSettings = Seq(
  version := "0.11.0",
  organization := "com.tethys-json",
  scalaVersion := "2.11.12",
  crossScalaVersions := Seq("2.11.12", "2.12.10", "2.13.1"),
  Compile / unmanagedSourceDirectories ++= {
    def extraDirs(suffix: String) = Seq(file(sourceDirectory.value.getPath + "/main/scala" + suffix))

    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, y)) if y <= 12 =>
        extraDirs("-2.12-")
      case Some((2, y)) if y >= 13 =>
        extraDirs("-2.13+")
      case _ => Nil
    }
  },
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
    ),
    Developer(
      id = "REDNBLACK",
      name = "Boris Potepun",
      email = "boris@f0w.org",
      url = url("https://github.com/REDNBLACK")
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

lazy val testSettings = Seq(
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test
)

lazy val tethys = project.in(file("."))
  .settings(commonSettings)
  .dependsOn(core, `macro-derivation`, `jackson-backend`)
  .aggregate(core, `macro-derivation`, `jackson-backend`, json4s, circe, enumeratum)

lazy val modules = file("modules")

lazy val core = project.in(modules / "core")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-core",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, y)) if y >= 13 =>
          Seq(
            "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
          )
        case _ => Nil
      }
    }
  )

lazy val `macro-derivation` = project.in(modules / "macro-derivation")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-derivation",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
    )
  )
  .dependsOn(core)

lazy val `jackson-backend` = project.in(modules / "jackson-backend")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-jackson",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.10.1"
    )
  )
  .dependsOn(core)

lazy val circe = project.in(modules / "circe")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-circe",
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 11)) =>
          Seq("io.circe" %% "circe-core" % "0.12.0-M3")
        case _ =>
          Seq("io.circe" %% "circe-core" % "0.12.3")
      }
    }
  )
  .dependsOn(core, `jackson-backend` % Test)

lazy val json4s = project.in(modules / "json4s")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-json4s",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-core" % "3.6.7"
    )
  )
  .dependsOn(core)

lazy val enumeratum = project.in(modules / "enumeratum")
  .settings(commonSettings)
  .settings(testSettings)
  .settings(
    name := "tethys-enumeratum",
    libraryDependencies ++= Seq(
      "com.beachape" %% "enumeratum" % "1.5.14"
    )
  )
  .dependsOn(core)

lazy val benchmarks = project.in(modules / "benchmarks")
  .settings(commonSettings)
  .settings(
    publishTo := None,
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.5",
      "org.json4s" %% "json4s-native" % "3.6.7",
      "org.json4s" %% "json4s-jackson" % "3.6.7",
      "com.typesafe.play" %% "play-json" % "2.7.4",

      "org.knowm.xchart" % "xchart" % "3.6.0" exclude("de.erichseifert.vectorgraphics2d", "VectorGraphics2D") withSources()
    ),
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq(
          "io.circe" %% "circe-core" % "0.12.3",
          "io.circe" %% "circe-generic" % "0.12.3",
          "io.circe" %% "circe-jawn" % "0.12.3",
          "io.circe" %% "circe-jackson210" % "0.12.1"
        )
        case _             => Seq(
          "io.circe" %% "circe-core" % "0.12.0-M3",
          "io.circe" %% "circe-generic" % "0.12.0-M3",
          "io.circe" %% "circe-jawn" % "0.12.0-M3",
          "io.circe" %% "circe-jackson210" % "0.11.2"
        )
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Nil
        case _             => Seq(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
      }
    },
    scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, 13)) => Seq("-Ymacro-annotations")
        case _             => Nil
      }
    }
  )
  .dependsOn(core, `macro-derivation`, `jackson-backend`)
  .enablePlugins(JmhPlugin)