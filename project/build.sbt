
scalaVersion := "2.9.2"

resolvers += "releases"  at "http://oss.sonatype.org/content/repositories/releases"

resolvers += "maven" at "http://repo1.maven.org/maven2"

resolvers += Resolver.url("sbt-plugin-releases", new URL("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases/"))(Resolver.ivyStylePatterns)

resolvers += "sbt-idea-repo" at "http://mpeltonen.github.com/maven/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

//addSbtPlugin("org.specs2" % "specs2_2.9.2" % "1.11" % "test")

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.3.0-SNAPSHOT")

