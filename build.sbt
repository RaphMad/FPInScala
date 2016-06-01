name := "FPInScala"

version := "1.0"

scalaVersion := "2.11.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.6" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.0" % "test"

// force specific versions of libraries
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
