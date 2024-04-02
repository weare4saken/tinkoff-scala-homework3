val scala3Version = "2.13.13"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tinkoff-homework3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
    )
  )