name := "Competetive shapley"

version := "0.1"

scalaVersion := "2.10.2"

scalacOptions ++= Seq("-unchecked", "-deprecation")

libraryDependencies += "com.assembla.scala-incubator" % "graph-core_2.10" % "1.7.3"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "1.9.2" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "com.typesafe" %% "scalalogging-slf4j" % "1.0.1"

EclipseKeys.withSource := true

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.0.9"

lazy val logback = "ch.qos.logback" % "logback-classic" % "1.0.9"

parallelExecution in Test := false

javaOptions in run += "-Xmx4G"

// One jar sbt plugin settings
seq(com.github.retronym.SbtOneJar.oneJarSettings: _*)
