import sbt.Keys._

lazy val commonSettings = Seq(
  organization := "com.foomoo.abc",
  version := "0.4",
  scalaVersion := "2.11.7",
  licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt"))
)

lazy val publishSettings = Seq(
  publishMavenStyle := true,
  pomIncludeRepository := { _ => false },
  publishArtifact in Test := false,
  pomExtra :=
    <developers>
      <developer>
        <name>Daniel Watford</name>
        <email>dan@foomoo.com</email>
        <url>http://foomoo.com</url>
        <timezone>Europe/London</timezone>
      </developer>
    </developers>
)

lazy val dependencies = Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
  "org.json4s" %% "json4s-native" % "3.3.0",
  "org.scalatest" % "scalatest_2.11" % "2.2.6" % Test
)

lazy val `abc-parent` = (project in file("."))
  .aggregate(`abc-domain`, `abc-parser`, `abc-app`)
  .settings(commonSettings: _*)
  .settings(publishSettings)

lazy val `abc-domain` = (project in file("domain"))
  .settings(commonSettings: _*)
  .settings(publishSettings)
  .settings(
    name := "abc-domain",
    libraryDependencies ++= dependencies
  )

lazy val `abc-parser` = (project in file("parser"))
  .dependsOn(`abc-domain`)
  .settings(commonSettings: _*)
  .settings(publishSettings)
  .settings(
    name := "abc-parser",
    libraryDependencies ++= dependencies
  )

lazy val `abc-app` = (project in file("app"))
  .dependsOn(`abc-parser`)
  .settings(commonSettings: _*)
  .settings(
    name := "abc-app",
    libraryDependencies ++= dependencies
  )

