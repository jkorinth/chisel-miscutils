name := "chisel-miscutils"

organization := "esa.cs.tu-darmstadt.de"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.3", "2.10.4", "2.11.0")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map("chisel3"          -> "3.0-SNAPSHOT",
                          "chisel-iotesters" -> "1.1-SNAPSHOT")

libraryDependencies ++= (Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.4.8"
)

// no parallel tests

parallelExecution in Test := false

testForkedParallel in Test := false

scalacOptions ++= Seq("-language:implicitConversions", "-language:reflectiveCalls", "-deprecation", "-feature")

