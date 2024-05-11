ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name             := "cats",
    idePackagePrefix := Some("com.zibaldone")
  )

libraryDependencies += "org.typelevel" %% "cats-core" % "2.10.0"
