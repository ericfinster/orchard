name := "orchard-lib"

scalaVersion := "2.10.2"

scalacOptions += "-feature"

scalacOptions += "-deprecation"

excludeFilter in unmanagedSources ~= { _ || "ExpressionFramework.scala" }

