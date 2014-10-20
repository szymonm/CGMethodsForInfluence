name := "Competetive shapley"

version := "0.4"

scalaVersion := "2.11.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.11" % "1.9.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.9"

lazy val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"

libraryDependencies += "com.typesafe" % "config" % "1.2.1"

parallelExecution in Test := false

javaOptions in run += "-Xmx4G"

EclipseKeys.withSource := true
