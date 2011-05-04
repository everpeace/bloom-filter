import sbt._

class BloomFilterProject(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {

  val specsDependency = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7.2" % "test" withSources

  val scalacheckDependency = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" withSources

}
