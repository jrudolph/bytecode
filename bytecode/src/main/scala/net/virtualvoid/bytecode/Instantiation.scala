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
  def apply[R <: List](): F[R**T] => F[R**U] =
    withLocal { arg1 => f =>
      (f.new_int(constructor.getDeclaringClass) ~
        dup ~
        arg1.load)
        .invokeconstructor(this)
    }
}
case class Ctor2[T1, T2, U](override val constructor: jConstructor[_]) extends Constructor {
  override val numParams = 2

  import Bytecode.Instructions._
  def apply[R <: List](): F[R**T1**T2] => F[R**U] =
    withLocal { arg2 => _ ~ withLocal { arg1 => f =>
      (f.new_int(constructor.getDeclaringClass) ~
        dup ~
        arg1.load ~ arg2.load)
        .invokeconstructor(this)
    }}
}

trait InstantiationInstructions {
  def ctor1[T, U](c: Code[T => U]): Ctor1[T, U] = Ctor1[T, U](CodeTools.constructorFromTree(c.tree))
  def ctor2[T1, T2, U](c: Code[(T1, T2) => U]): Ctor2[T1, T2, U] = Ctor2[T1, T2, U](CodeTools.constructorFromTree(c.tree))
}
