package net.virtualvoid.bytecode

import scala.reflect.Code
import java.lang.reflect.{ Constructor => jConstructor }                        

trait Uninitialized[T]

trait Constructor {
  def constructor: jConstructor[_]
  def numParams: Int
}

case class Ctor1[T, U <: AnyRef](override val constructor: jConstructor[_]) extends Constructor {
  override val numParams = 1

  import Bytecode.Instructions._
  import LowLevelInstantiationInstructions._

  def apply[R <: List](): F[R**T] => F[R**U] =
    withLocal { arg1 => 
      _ ~ 
        newUninitialized(constructor.getDeclaringClass.asInstanceOf[Class[U]]) ~
        dup ~
        arg1.load ~
        invokeconstructor(this)
    }
}
case class Ctor2[T1, T2, U <: AnyRef](override val constructor: jConstructor[_]) extends Constructor {
  override val numParams = 2

  import Bytecode.Instructions._
  import LowLevelInstantiationInstructions._

  def apply[R <: List](): F[R**T1**T2] => F[R**U] =
    withLocal { arg2 => _ ~ withLocal { arg1 =>
      _ ~
        newUninitialized(constructor.getDeclaringClass.asInstanceOf[Class[U]]) ~
        dup ~
        arg1.load ~ arg2.load ~
        invokeconstructor(this)
    }}
}

trait InstantiationInstructions {
  def ctor1[T, U <: AnyRef](c: Code[T => U]): Ctor1[T, U] = Ctor1[T, U](CodeTools.constructorFromTree(c.tree))
  def ctor2[T1, T2, U <: AnyRef](c: Code[(T1, T2) => U]): Ctor2[T1, T2, U] = Ctor2[T1, T2, U](CodeTools.constructorFromTree(c.tree))
}

object LowLevelInstantiationInstructions {
  def newUninitialized[R <: List, T <: AnyRef](cl: Class[T]): F[R] => F[R**Uninitialized[T]] = f => f.new_int(cl)
  def invokeconstructor[ST <: List, R <: List, U <: AnyRef](cons: Constructor): F[ST] => F[R**U] = f => f.invokeconstructor(cons)
}
