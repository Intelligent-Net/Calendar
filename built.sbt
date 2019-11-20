name := "Universal Calendar"
scalaVersion := "2.12.10"
logBuffered in Test := false

libraryDependencies ++= Seq(
"org.scalactic" %% "scalactic" % "3.0.8",
"org.scalatest" %% "scalatest" % "3.0.8" % Test
)
