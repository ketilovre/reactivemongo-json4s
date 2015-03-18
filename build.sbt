name := """reactivemongo-json4s"""

organization := "com.ketilovre"

version := "0.1.0"

scalaVersion := "2.11.5"

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-Xfatal-warnings"
)

scalacOptions in Test ++= Seq(
  "-Yrangepos"
)

resolvers ++= Seq(
  "Typesafe repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.reactivemongo" %% "reactivemongo"      % "0.10.5.0.akka23",
  "org.json4s"        %% "json4s-jackson"     % "3.2.11",
  "org.specs2"        %% "specs2-core"        % "2.4.15"            % "test",
  "org.specs2"        %% "specs2-scalacheck"  % "2.4.15"            % "test"
)

parallelExecution in Test := false