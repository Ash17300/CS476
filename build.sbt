ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "HW1"
  )


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test

fork in Test := true
javaOptions in Test ++= Seq("-Xmx2G", "-XX:MaxMetaspaceSize=512M")