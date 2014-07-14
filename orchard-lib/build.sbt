name := "orchard-lib"

scalaVersion := "2.10.2"

scalacOptions ++= List("-feature", "-deprecation")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "org.scalaz" %% "scalaz-concurrent" % "7.0.6"
)

excludeFilter in unmanagedSources ~= { _ || "ExpressionFramework.scala" }

