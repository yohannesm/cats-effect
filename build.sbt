val scala2Version = "2.13.7"
val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "cats-effect",
    version := "0.1.0",
    scalaVersion := scala2Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.2.0"
    )
  )
