/**
  * Build.scala - Orchard build specification
  * 
  * @author Eric Finster
  * @version 0.1 
  */

import sbt._

object OrchardBuild extends Build {

  lazy val orchardLib = project.in(file("orchard-lib"))
  lazy val orchardJavafx = project.in(file("orchard-javafx")).dependsOn(orchardLib)
  lazy val orchardVaadin = project.in(file("orchard-vaadin")).dependsOn(orchardLib)

}
