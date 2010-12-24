import sbt._

class ObjectFormatter(info: ProjectInfo) extends DefaultProject(info) {
  val snapshots = ScalaToolsSnapshots
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.6" % "test"
  val mnemonics = "virtualvoid" %% "mnemonics" % "2.2-SNAPSHOT"
}
