val http4sVersion = "0.23.26"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2"
libraryDependencies += "org.typelevel" %% "kittens" % "3.2.0"
libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.4.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "3.10.0"
libraryDependencies += "org.http4s" %% "http4s-ember-client" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-ember-server" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-dsl" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-dsl" % http4sVersion
libraryDependencies += "org.http4s" %% "http4s-circe" % http4sVersion
libraryDependencies += "io.circe" %% "circe-core" % "0.14.7"
libraryDependencies += "io.circe" %% "circe-parser" % "0.14.7"
libraryDependencies += "io.circe" %% "circe-generic" % "0.14.7"
libraryDependencies += "io.circe" %% "circe-literal" % "0.14.7"
libraryDependencies += "org.typelevel" %% "log4cats-slf4j" % "2.3.1"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.6"
libraryDependencies += "com.disneystreaming" %% "weaver-cats" % "0.8.4" % Test
libraryDependencies += "com.disneystreaming" %% "weaver-scalacheck" % "0.8.4" % Test

testFrameworks += new TestFramework("weaver.framework.CatsEffect")

// val circeVersion = "0.14.1"
//
// libraryDependencies ++= Seq(
//   "io.circe" %% "circe-core",
//   "io.circe" %% "circe-generic",
//   "io.circe" %% "circe-parser"
// ).map(_ % circeVersion)

scalaVersion := "3.3.1"

watchTriggeredMessage := ((a, b, c) => None)
watchStartMessage := ((a, b, c) => None)

Global / onChangedBuildSource := ReloadOnSourceChanges
Compile / run / fork := true

connectInput in run := true
