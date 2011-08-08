import sbt._
import de.element34.sbteclipsify._

class Mnemonics(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
  val snapshots = ScalaToolsSnapshots
  val asm = "asm" % "asm" % "3.1"
  val specs = "org.scala-tools.testing" %% "specs" % "1.6.8" % "test"
}
