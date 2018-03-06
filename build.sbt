name := "chisel-miscutils"

organization := "esa.cs.tu-darmstadt.de"

version := "0.5-SNAPSHOT"

scalaVersion := "2.12.4"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map("chisel3"          -> "3.0.2",
                          "chisel-iotesters" -> "1.1.2")

libraryDependencies ++= (Seq("chisel3","chisel-iotesters").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.5" % "test",
  "com.typesafe.play" %% "play-json" % "2.6.9"
)

// no parallel tests

parallelExecution in Test := false

testForkedParallel in Test := false

scalacOptions ++= Seq("-language:implicitConversions", "-language:reflectiveCalls", "-deprecation", "-feature")

cleanFiles += baseDirectory.value / "test"

