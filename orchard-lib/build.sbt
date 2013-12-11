name := "orchard-lib"

scalaVersion := "2.10.2"

scalacOptions += "-feature"

excludeFilter in unmanagedSources ~= { _ || "MutableComplex.scala" || "Panel.scala" || "Gallery.scala" || "RenderingPanel.scala" || "SimpleCardinalComplex.scala" || "CardinalComplex.scala" || "MutablePanel.scala" || "SimpleFramework.scala" || "ExpressionFramework.scala" || "CellComplex.scala" || "ExpressionBuilderComplex.scala" || "SimpleMutableComplex.scala" || "Examples.scala"  || "Expression.scala" }
