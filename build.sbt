libraryDependencies ++= Seq(
  "asm" % "asm" % "3.1",
  "org.scala-tools.testing" %% "specs" % "1.6.9" % "test"
)

initialCommands in console +=
  """import net.virtualvoid.bytecode._
     |import Methods._
     |import Bytecode.Instructions._
     |import backend.ASM""".stripMargin