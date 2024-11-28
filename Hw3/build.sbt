// build.sbt
name := "HW1"

version := "0.1" //project version

scalaVersion := "3.3.0"//scala version used

javaHome := Some(file("C:/Program Files/Java/jdk-17"))//setting home directory to java 17 as i getting errors
//adding scalatest as dependancies
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.18" % Test
)

fork := true
//compiler option for the right java version
Compile / javacOptions ++= Seq("--release", "17")
Test / javacOptions ++= Seq("--release", "17")

Test / fork := true
Test / javaHome := Some(file("C:/Program Files/Java/jdk-17"))
