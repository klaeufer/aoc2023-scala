name := "aoc2023"

version := "0.1"

scalaVersion := "3.3.1"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Yexplicit-nulls", "-Ysafe-init", "-language:strictEquality")

coverageEnabled := true

libraryDependencies ++= Seq(
  "org.scalanlp" %% "breeze" % "2.1.0",
  "org.scalatest" %% "scalatest"  % "3.2.17"  % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test"
)

logBuffered := false

Test / parallelExecution := false

enablePlugins(JavaAppPackaging)

