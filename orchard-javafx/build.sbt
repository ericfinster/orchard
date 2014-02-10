name := "orchard-javafx"

scalaVersion := "2.10.2"

unmanagedJars in Compile := {
  val base = baseDirectory.value
  val customJars = (base ** "*.jar") +++ (file("/opt/java/jre/lib/jfxrt.jar"))
  customJars.classpath
}

excludeFilter in unmanagedSources ~= { _ || "SimpleEditor.scala" || "JavaFXDefinitionBuilder.scala" }

unmanagedJars in Compile += Attributed.blank(
    file(scala.util.Properties.javaHome) / "lib" / "jfxrt.jar")

scalacOptions += "-feature"

fork := true
