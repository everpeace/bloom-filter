import sbt._

class BloomFilterProject(info: ProjectInfo) extends DefaultProject(info) with IdeaProject {

  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  val scalazCore = "com.googlecode.scalaz" %% "scalaz-core" % "5.1-SNAPSHOT" withSources

  val specsDependency = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7.2" % "test" withSources

  val scalacheckDependency = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" withSources

}
