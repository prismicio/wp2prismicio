name := "wp2prismic"

scalaVersion := "2.11.4"

resolvers += "Typesafe Releases" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "com.typesafe.play" %% "play-json" % "2.4.0-M2",
  "com.typesafe.play" %% "play-json" % "2.4.0-M2",
  "org.jsoup" % "jsoup" % "1.8.1"
)
