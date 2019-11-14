lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  version := "0.0.0",
  scalaVersion := "2.12.8",
  libraryDependencies += scalaTest,
  fork := true
)

lazy val scalaTest  = "org.scalatest" %% "scalatest" % "3.0.5" % Test

lazy val root = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "binval-complexity")
