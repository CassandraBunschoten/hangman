lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.13.8"
    )),
    name := "hangman",
    libraryDependencies ++= Seq(
        "dev.zio" %% "zio"         % "1.0.12",
        "dev.zio" %% "zio-streams" % "1.0.12"
    ),
    trapExit := false
  )