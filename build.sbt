lazy val root = project
  .in(file("."))
  .settings(
    name := "challah",
    version := "0.1.0",

    scalaVersion := "3.1.3",
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test := baseDirectory.value / "test",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % Test,
  )
