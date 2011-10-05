import sbt._
import de.element34.sbteclipsify._

class Mnemonics(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
  val snapshots = ScalaToolsSnapshots
  val asm = "asm" % "asm" % "3.1"
  val specs = "org.scala-tools.testing" % "specs_2.9.0-1" % "1.6.8" % "test"

  override def consoleInit =
    """import net.virtualvoid.bytecode._
      |import Methods._
      |import Bytecode.Instructions._
      |import backend.ASM
    """.stripMargin
}
