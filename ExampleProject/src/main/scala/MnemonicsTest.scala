import net.virtualvoid.bytecode._
import Methods._
import Bytecode.Instructions._
import backend.ASM

import java.lang.Integer

object MnemonicsTest extends App {
  val f = ASM.compile(classOf[Integer], classOf[Integer])(
	param => ret => frame => frame ~
	  param.load ~
	  method((x: Integer) => x.intValue) ~
	  bipush(1) ~
	  iadd ~
	  method((x: Int) => Integer.valueOf(x)) ~
	  ret . jmp)

  println("f(5) = "+f(5))
}
