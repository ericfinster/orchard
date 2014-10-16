/**
  * Build.scala - Build file for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

import sbt._
import Keys._
import play.Play._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import com.typesafe.sbt.packager.universal.UniversalKeys

object ApplicationBuild extends Build with UniversalKeys {

  val orchardJsOutputDir = Def.settingKey[File]("directory for javascript files output by scalajs")

  override def rootProject = Some(orchardPlay)

  val sharedSrcDir = "orchard-common"

  lazy val orchardPlay = Project(
    id = "orchard-play",
    base = file("orchard-play")
  ) enablePlugins (play.PlayScala) settings (orchardPlaySettings: _*) aggregate (orchardJs) dependsOn (orchardLib)

  lazy val orchardJavaFX = Project(
    id = "orchard-javafx",
    base = file("orchard-javafx")
  ) settings (orchardJavaFXSettings: _*) dependsOn (orchardLib)

  lazy val orchardJs = Project(
    id   = "orchard-js",
    base = file("orchard-js")
  ) settings (orchardJsSettings: _*)

  lazy val orchardLib = Project(
    id   = "orchard-lib",
    base = file("orchard-lib")
  ) settings (orchardLibSettings: _*)

  lazy val orchardCommon = Project(
    id = "orchardCommon",
    base = file(sharedSrcDir)
  ) settings (orchardCommonSettings: _*)

  lazy val orchardPlaySettings =
    Seq(
      name := "orchard-play",
      version := Versions.app,
      scalaVersion := Versions.scala,
      orchardJsOutputDir := (crossTarget in Compile).value / "classes" / "public" / "javascripts",
      compile in Compile <<= (compile in Compile) dependsOn (fastOptJS in (orchardJs, Compile)),
      dist <<= dist dependsOn (fullOptJS in (orchardJs, Compile)),
      libraryDependencies ++= Dependencies.orchardPlay,
      commands += preStartCommand
    ) ++ (
      // ask scalajs project to put its outputs in scalajsOutputDir
      Seq(packageExternalDepsJS, packageInternalDepsJS, packageExportedProductsJS, packageLauncher, fastOptJS, fullOptJS) map { packageJSKey =>
        crossTarget in (orchardJs, Compile, packageJSKey) := orchardJsOutputDir.value
      }
    )

  lazy val orchardJavaFXSettings = 
    Seq(
      name := "orchard-javafx",
      version := Versions.app,
      scalaVersion := Versions.scala,
      scalacOptions ++= Seq("-feature", "-deprecation"),
      libraryDependencies ++= Dependencies.orchardJavafx,
      unmanagedJars in Compile += Attributed.blank(
        file(scala.util.Properties.javaHome) / "lib" / "jfxrt.jar"),
      Keys.fork := true
    )

  lazy val orchardJsSettings =
    scalaJSSettings ++ Seq(
      name := "orchard-js",
      version := Versions.app,
      scalaVersion := Versions.scala,
      persistLauncher := true,
      persistLauncher in Test := false,
      libraryDependencies ++= Seq(
        "org.scala-lang.modules.scalajs" %%% "scalajs-jquery" % Versions.scalajsJQuery,
        "com.github.japgolly.fork.scalaz" %%% "scalaz-core" % "7.1.0",
        "com.scalatags" %%% "scalatags" % Versions.scalaTags
      )
    ) ++ sharedDirectorySettings

  lazy val orchardLibSettings = 
    Seq(
      name := "orchard-lib",
      version := Versions.app,
      scalaVersion := Versions.scala,
      scalacOptions ++= Seq("-feature", "-deprecation"),
      excludeFilter in unmanagedSources ~= { _ || "svg*" || "expression*" },
      libraryDependencies ++= Dependencies.orchardLib
    ) ++ sharedDirectorySettings

  lazy val orchardCommonSettings =
    Seq(
      name := "orchard-common",
      libraryDependencies ++= Dependencies.shared
    )

  lazy val sharedDirectorySettings = Seq(
    unmanagedSourceDirectories in Compile += new File((file(".") / sharedSrcDir / "src" / "main" / "scala").getCanonicalPath),
    unmanagedSourceDirectories in Test += new File((file(".") / sharedSrcDir / "src" / "test" / "scala").getCanonicalPath),
    unmanagedResourceDirectories in Compile += file(".") / sharedSrcDir / "src" / "main" / "resources",
    unmanagedResourceDirectories in Test += file(".") / sharedSrcDir / "src" / "test" / "resources"
  )

  // Use reflection to rename the 'start' command to 'play-start'
  Option(play.Play.playStartCommand.getClass.getDeclaredField("name")) map { field =>
    field.setAccessible(true)
    field.set(playStartCommand, "play-start")
  }

  // The new 'start' command optimises the JS before calling the Play 'start' renamed 'play-start'
  val preStartCommand = Command.args("start", "<port>") { (state: State, args: Seq[String]) =>
    Project.runTask(fullOptJS in (orchardJs, Compile), state)
    state.copy(remainingCommands = ("play-start " + args.mkString(" ")) +: state.remainingCommands)
  }
}

object Dependencies {

  val shared = Seq(
    "org.scalaz" %% "scalaz-core" % "7.1.0"
  )

  val orchardPlay = Seq() ++ shared
  val orchardLib = Seq() ++ shared

  val orchardJavafx = Seq(
    "org.scalafx" %% "scalafx" % "2.2.67-R10"
  ) ++ shared

}

object Versions {
  val app = "0.1.0-SNAPSHOT"
  val scala = "2.11.1"
  val scalajsJQuery = "0.6"
  val scalaTags = "0.3.8"
}
