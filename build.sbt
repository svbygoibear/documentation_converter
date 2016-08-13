name := "documentation-converter"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= {
  val scalazVersion = "7.2.0"
  val akkaVersion = "2.4.1"
  Seq(
    "ch.qos.logback" % "logback-core" % "1.1.3",
    "ch.qos.logback" % "logback-classic" % "1.1.3",
    "org.slf4j" % "slf4j-api" % "1.7.12",
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "org.scalaz" %% "scalaz-core" % scalazVersion,
    "com.jsuereth" %% "scala-arm" % "1.4",
    "org.scala-lang" % "scala-compiler" % scalaVersion.value,
    "org.scala-lang" % "scala-reflect" % scalaVersion.value
  )
}

    