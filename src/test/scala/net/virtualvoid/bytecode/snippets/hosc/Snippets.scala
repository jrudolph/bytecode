package net.virtualvoid.bytecode
package snippets.hosc

import org.specs.Specification

import Methods._
import Bytecode.Instructions._
import backend.ASM

object Snippets extends Specification {
  "Examples from the article" should {
    "compile and execute correctly" in {
      "figure4" in {
        val f = ASM.compile(
          classOf[Integer], classOf[Integer])(
          param => ret => frame => frame ~
            param.load ~
            method((x: Integer) => x.intValue) ~
            bipush(1) ~
            iadd ~
            method((x: Int) => Integer.valueOf(x)) ~
            ret.jmp)

        f(0) must be_==(1)
        f(12) must be_==(13)
      }

      "figure6" in {
        def fCountdown[ST <: Stack] = ((f: F[ST]) =>
          f ~ bipush(5) ~ withTargetHere((target: Target[ST**Int]) =>
            (_: F[ST**Int]) ~ dup ~ ifne(_ ~
              dup ~
              method(printme(_: Int)) ~
              bipush(1) ~
              isub ~
              target.jmp)) ~
            method(Integer.valueOf(_: Int)))

        val code =
          ASM.compile(classOf[Integer], classOf[Integer])( param => ret =>
            _ ~ fCountdown ~ ret.jmp
          )

        resetPrintBuffer
        code(28) must be_==(0)
        printBuffer must be_==(Seq(5, 4, 3, 2, 1))
      }

      "chapter41Example" in {
        val code =
          ASM.compile(classOf[Integer], classOf[Integer])(param => ret => _ ~
            param.load ~
            method((_: Integer).intValue) ~
            iterate[Nil ** Int, Nil ** Int](self => _ ~
              dup ~
              method(printme(_: Int)) ~
              bipush(1) ~ isub ~ dup ~
              ifne2(self, nop)
            ) ~
            method(Integer.valueOf(_: Int)) ~
            ret.jmp)

        resetPrintBuffer
        code(13) must be_==(0)
        printBuffer must be_==(Seq(13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1))
      }

      "chapter66Example" in {
        val code =
          ASM.compile(classOf[Integer], classOf[String])(param => ret => _ ~
            param.load ~
            method((_: Integer).intValue) ~ bipush(5) ~ isub ~
            ifne(_ ~ ldc(" does not equal 5") ~
              ret.jmp) ~
            ldc(" equals 5") ~
            ret.jmp)

        code(5) must be_==(" equals 5")
        code(12) must be_==(" does not equal 5")
      }
    }
  }

  val printBuffer = collection.mutable.Buffer.empty[Int]
  def resetPrintBuffer {
    printBuffer.clear
  }
  def printme (i: Int ){
    //System.out.println (i)
    printBuffer += i
  }
}