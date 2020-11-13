name := "gastro"

version := "0.1"

scalaVersion := "2.13.3"

val AkkaVersion = "2.6.10"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % AkkaVersion,
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)