name := "ConnectAPI Test #1"
 
version := "1.0"
 
scalaVersion := "2.10.0"
 
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "1.1.4"

libraryDependencies += "io.spray" %%  "spray-json" % "1.3.1"