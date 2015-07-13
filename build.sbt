import spray.revolver.RevolverPlugin.Revolver

name := """gaming-exercise-microservice"""

description := "This is a Akka HTTP microservice application"

organization := "com.company"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

libraryDependencies ++= {
  val akkaV       = "2.3.11"
  val akkaStreamV = "1.0-RC4"
  val scalaTestV  = "2.2.4"
  Seq(
    "org.scalaz"        %% "scalaz-core"                          % "7.1.3",
    "com.typesafe.akka" %% "akka-actor"                           % akkaV,
    "com.typesafe.akka" %% "akka-stream-experimental"             % akkaStreamV,
    "com.typesafe.akka" %% "akka-http-core-experimental"          % akkaStreamV,
    "com.typesafe.akka" %% "akka-http-experimental"               % akkaStreamV,
    "com.typesafe.akka" %% "akka-http-spray-json-experimental"    % akkaStreamV,
    "com.typesafe.akka" %% "akka-http-testkit-experimental"       % akkaStreamV,
    "org.scalatest"     %% "scalatest"                            % scalaTestV % "test"
  )
}

Revolver.settings


