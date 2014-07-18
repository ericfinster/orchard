name := "Orchard root project"

version := "0.1"

scalaVersion := "2.11.1"

lazy val root = project.in(file(".")).aggregate()

lazy val orchardLib = project.in(file("orchard-lib")).settings(
  name := "orchard-lib",
  unmanagedSourceDirectories in Compile += (baseDirectory in root).value / "orchard-common" / "src" / "main" / "scala",
  excludeFilter in unmanagedSources ~= { _ || "svg*" },
  libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "org.scalaz" %% "scalaz-concurrent" % "7.0.6"
  )
)

lazy val orchardJs = project.in(file("orchard-js")).settings(scalaJSSettings: _*).settings(
  name := "orchard-js",
  unmanagedSourceDirectories in Compile += (baseDirectory in root).value / "orchard-common" / "src" / "main" / "scala"
)

lazy val orchardLift = project.in(file("orchard-lift")).settings(webSettings: _*).settings(
  name := "orchard-lift",
  libraryDependencies ++= Seq(
    "net.liftweb" %% "lift-webkit" % "2.5.1" % "compile",
    "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910" % "container,test",
    "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" %
      "container,compile" artifacts Artifact("javax.servlet", "jar", "jar")
  )
).dependsOn(orchardLib)

lazy val orchardJavafx = project.in(file("orchard-javafx")).settings(
  name := "orchard-javafx",
  unmanagedJars in Compile += Attributed.blank(
    file(scala.util.Properties.javaHome) / "lib" / "jfxrt.jar"),
  fork := true
).dependsOn(orchardLib).dependsOn(orchardJs)


