ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "boundingbox",
    mainClass:= Some("Engine.scala"),
    libraryDependencies +=  "co.fs2" %% "fs2-io" % "3.9.2"
  )
