lazy val common_settings = Seq(
  organization := "problem233.github.io",
  version := "0",
  scalaVersion := "2.12.1",
  scalacOptions ++= Seq(
    "-Ywarn-adapted-args"
  )
)

lazy val root = project in file(".") settings(common_settings: _*) settings(
  name := "default",
  libraryDependencies ++= Seq(
    "org.spire-math" %% "spire" % "0.13.0",
    "org.scala-lang" % "scala-reflect" % "2.12.1"
  )
)
