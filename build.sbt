name := "chisel-miscutils"

organization := "esa.cs.tu-darmstadt.de"

version := "0.2-SNAPSHOT"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.3", "2.10.4", "2.11.0")

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "chisel" % "latest.release",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test",
  "com.typesafe.play" %% "play-json" % "2.4.8"
)

// no parallel tests

parallelExecution in Test := false

testForkedParallel in Test := false

