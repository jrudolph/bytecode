name := "mnemonics"

organization := "net.virtual-void"

version := "1.0.0-SNAPSHOT"

homepage := Some(url("http://github.com/jrudolph/bytecode"))

licenses in GlobalScope += "BSD 2-Clause License" -> url("http://www.opensource.org/licenses/BSD-2-Clause")

(LsKeys.tags in LsKeys.lsync) := Seq("jvm", "bytecode", "asm")

(LsKeys.docsUrl in LsKeys.lsync) <<= homepage

(description in LsKeys.lsync) :=
  "Typesafe bytecode combination for runtime code generation with Scala"
