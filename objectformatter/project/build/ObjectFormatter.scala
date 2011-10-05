import sbt._

class ObjectFormatter(info: ProjectInfo) extends DefaultProject(info) {
  val snapshots = ScalaToolsSnapshots
  val specs = "org.scala-tools.testing" % "specs_2.9.0-1" % "1.6.8" % "test"
  val mnemonics = "virtualvoid" %% "mnemonics" % "2.3-SNAPSHOT"
}
