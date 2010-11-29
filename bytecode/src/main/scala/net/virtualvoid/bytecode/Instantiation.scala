package net.virtualvoid.bytecode

import scala.reflect.Code
import java.lang.reflect.{ Constructor => jConstructor }                        

trait Constructor {
  def constructor: jConstructor[_]
  def numParams: Int
}

case class Ctor1[T, U](override val constructor: jConstructor[_]) extends Constructor {
  override val numParams = 1

  import Bytecode.Instructions._
  def apply[R <: List](/*arg1: F[Nil] => F[Nil**T]*/): F[R**T] => F[R**U] =
    withLocal { arg1 => f =>
      (f.new_int(constructor.getDeclaringClass) ~
        dup ~
        arg1.load)
        .invokeconstructor(this)
    }
}

trait InstantiationInstructions {
  def ctor1[T, U](c: Code[T => U]): Ctor1[T, U] = Ctor1[T, U](CodeTools.constructorFromTree(c.tree))
}
