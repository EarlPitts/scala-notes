libraryDependencies += "org.typelevel" %% "cats-core" % "2.9.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.2"
libraryDependencies += "org.typelevel" %% "kittens" % "3.2.0"
libraryDependencies += "com.github.j-mie6" %% "parsley" % "4.4.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "3.10.0"

scalaVersion := "3.3.3" 

watchTriggeredMessage := ((a, b, c) => None)
watchStartMessage     := ((a, b, c) => None)

Compile / run / fork := true
