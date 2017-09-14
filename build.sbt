val commonDependencies = Seq(
  version := "0.6.0",
  organization := "tethys",
  scalaVersion := "2.11.8",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  )
)

lazy val core = project.in(file("./modules/core"))
  .settings(commonDependencies)
  .settings(
  name := "tethys-core"
)

lazy val `macro-derivation` = project.in(file("./modules/macro-derivation"))
  .settings(commonDependencies)
  .settings(
    name := "tethys-macro-derivation",
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
    )
  ).dependsOn(core)

lazy val `jackson-backend` = project.in(file("./modules/jackson-backend"))
  .settings(commonDependencies)
  .settings(
    name := "tethys-jackson-backend",
    libraryDependencies ++= Seq(
      "com.fasterxml.jackson.core" % "jackson-core" % "2.9.1",
      "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
    )
  ).dependsOn(core)