name := "orchard-vaadin"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
  "com.vaadin" % "vaadin-server" % "7.1.9",
  "com.vaadin" % "vaadin-themes" % "7.1.9" % "container",
  "com.vaadin" % "vaadin-client-compiled" % "7.1.9" % "container",
  "com.vaadin" % "vaadin-push" % "7.1.9", //% "container",
  "javax.servlet" % "servlet-api" % "2.4",
  "org.vaadin.addons" % "scaladin" % "3.0.0",
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.12.v20130726" % "container",
  "org.eclipse.jetty" % "jetty-websocket" % "8.1.12.v20130726" % "container"
)

vaadinWebSettings
