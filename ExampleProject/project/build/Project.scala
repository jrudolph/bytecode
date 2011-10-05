import sbt._
import de.element34.sbteclipsify._

class Mnemonics(info: ProjectInfo) extends DefaultProject(info) with Eclipsify {
  val mnemonics = "virtualvoid" %% "mnemonics" % "2.3-SNAPSHOT"

  override def consoleInit =
    """import net.virtualvoid.bytecode._
      |import Methods._
      |import Bytecode.Instructions._
      |import backend.ASM
    """.stripMargin
}
