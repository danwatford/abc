import sbt.Keys._

lazy val commonSettings = Seq(
  organization := "com.foomoo.abc",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.11.7",

  licenses := Seq("Apache License, Version 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false }
)

lazy val dependencies = Seq(
  "org.scala-lang.modules" % "scala-parser-combinators_2.11" % "1.0.4",
  "org.json4s" %% "json4s-native" % "3.3.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % Test
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(publishSettings).
  settings(
    name := "abc-parser",
    libraryDependencies ++= dependencies
  )
