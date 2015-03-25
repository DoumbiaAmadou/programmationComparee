name := "pcomp"
version := "1.0"
scalaVersion := "2.11.2"
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-actor" % "2.3.9",
	"net.liftweb" % "lift-webkit_2.11" % "3.0-M5-1"
)
