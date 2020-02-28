ThisBuild / scalaVersion := "2.13.0"

lazy val root = (project in file("."))
  .settings(
    name := "goose",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test,
    assemblyJarName in assembly := s"${name.value}.jar"
  )
