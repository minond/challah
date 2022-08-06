lazy val root = project
  .in(file("."))
  .settings(
    name := "challah",
    version := "0.1.0",

    scalaVersion := "3.1.3",
    scalacOptions := Seq("-deprecation"),

    Compile / scalaSource := baseDirectory.value / "src",
    Test / scalaSource := baseDirectory.value / "test",

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.13" % Test,
  )
