name := "Acide Fourmique"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "net.liftweb" %% "lift-json" % "3.0-M1"
)

scalacOptions += "-deprecation"
