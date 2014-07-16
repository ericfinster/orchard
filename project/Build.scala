/**
  * Build.scala - Orchard build specification
  * 
  * @author Eric Finster
  * @version 0.1 
  */

import sbt._
import Keys._

import sbtassembly.Plugin._ 
import AssemblyKeys._

object OrchardBuild extends Build {

  lazy val root = Project(
    id = "orchard", 
    base = file("."),
    dependencies = Seq(orchardJavafx)
  ).settings(
    name := "orchard",
    version := "0.1-SNAPSHOT",
    mainClass in (Compile, run) := Some("orchard.ui.javafx.Editor")
  ).aggregate(orchardLib, orchardJavafx)

  lazy val orchardLib = Project(
    id = "orchard-lib",
    base = file("orchard-lib")
  )

  lazy val orchardJavafx = Project(
    id = "orchard-javafx",
    base = file("orchard-javafx"),
    dependencies = Seq(orchardLib)
  ).settings(buildSettings: _*).
    settings(assemblySettings: _*).
    settings( 
      mainClass := Some("orchard.ui.javafx.Editor"),
      excludedJars in assembly <<= (fullClasspath in assembly) map { cp =>
        cp filter {_.data.getName == "jfxrt.jar"} },
      jarName in assembly := "orchard-0.1-SNAPSHOT.jar"
    )

  lazy val orchardLift = Project(
    id = "orchard-lift",
    base = file("orchard-lift"),
    dependencies = Seq(orchardLib)
  )

}
