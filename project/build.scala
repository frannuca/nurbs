import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import sbt._
import Keys._
import scalariform.formatter.preferences._

object BuildSettings {


  val buildOrganization = "org.fjn"
  val buildVersion      = "1.0.0"
  val buildScalaVersion = "2.9.2"

  ScalariformKeys.preferences := FormattingPreferences()
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 120)
    .setPreference(CompactControlReadability, false)
    .setPreference(PreserveDanglingCloseParenthesis, true)
    .setPreference(FormatXml, false)
    .setPreference(PreserveSpaceBeforeArguments, true)
    .setPreference(IndentWithTabs, false)
    .setPreference(SpacesWithinPatternBinders, false)

  val buildSettings = Defaults.defaultSettings ++ SbtScalariform.scalariformSettings ++ Seq(
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion //,
  )
}

object Resolvers {
  "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
}

object Dependencies {

  def SurfacePlotter = "net.ericaro" % "surfaceplotter" % "2.0.2-SNAPSHOT"

  val breezeVersion = "0.2-SNAPSHOT"

  def BreezeMath = "org.scalanlp" %% "breeze-math" % breezeVersion

  def BreezeLearn = "org.scalanlp" %% "breeze-learn" % breezeVersion

  def BreezeProcess = "org.scalanlp" %% "breeze-process" % breezeVersion

  def BreezeViz = "org.scalanlp" %% "breeze-viz" % breezeVersion

  def Blas = "org.scalanlp" % "jblas" % "1.2.1"
}

object PythiaBuild extends Build {

  import Resolvers._
  import Dependencies._
  import BuildSettings._

  /**
   * top layer  pythia
   */
  lazy val pythia = Project(
    "nurbs",
    file("."),
    settings = buildSettings ++ Seq(
      resolvers := Seq(Resolver.sonatypeRepo("snapshots")),
      libraryDependencies ++= Seq(SurfacePlotter, BreezeMath, BreezeLearn, BreezeProcess, BreezeViz,Blas))

  ) //aggregate (optimizer,ia, fjn.fjn.fjn.pythia.pricers)


}
