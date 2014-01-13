resolvers += Resolver.url("scala-js-releases",
    url("http://repo.scala-js.org/repo/releases/"))(Resolver.ivyStylePatterns)

addSbtPlugin("org.scala-lang.modules.scalajs" % "scalajs-sbt-plugin" % "0.2")

addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "0.4.2")


