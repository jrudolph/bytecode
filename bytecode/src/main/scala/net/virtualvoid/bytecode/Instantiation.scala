package net.virtualvoid.bytecode

import scala.reflect.Code
import java.lang.reflect.{ Constructor => jConstructor }                        

trait Uninitialized[T <: AnyRef]

trait Constructor {
  def constructor: jConstructor[_]
  def numParams: Int
}

class Constructor2[Args <: List, U <: AnyRef](override val constructor: jConstructor[_], override val numParams: Int) extends Constructor {
  def lowLevelApply[R <: List, Res1 <: List, Res2 <: List](implicit s1: Sub[R, Args, Res1], s2: Sub[Res1, Nil**Uninitialized[U], Res2]): F[R] => F[Res2**U] = null
  def apply[R <: List, Res <: List]()(implicit s: Sub[R, Args, Res]): F[R] => F[Res**U] = null
    
}

/*case class Ctor1[T, U <: AnyRef](override val constructor: jConstructor[_]) extends Constructor2[Nil**Uninitialized[U]**T, U] {
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
case class Ctor2[T1, T2, U <: AnyRef](override val constructor: jConstructor[_]) extends Constructor2[Nil**Uninitialized[U]**T1**T2, U] {
  override val numParams = 2

  import Bytecode.Instructions._
  def apply[R <: List](): F[R**T1**T2] => F[R**U] =
    withLocal { arg2 => _ ~ withLocal { arg1 => f =>
      (f.new_int(constructor.getDeclaringClass) ~
        dup ~
        arg1.load ~ arg2.load)
        .invokeconstructor(this)
    }}
}*/

trait InstantiationInstructions {
  def ctor1[T, U <: AnyRef](c: Code[T => U])/*: Ctor1[T, U]*/ = new Constructor2[Nil**T, U](CodeTools.constructorFromTree(c.tree), 1)
  def ctor2[T1, T2, U <: AnyRef](c: Code[(T1, T2) => U])/*: Ctor2[T1, T2, U]*/ = new Constructor2[Nil**T1**T2, U](CodeTools.constructorFromTree(c.tree), 2)
}

trait LowLevelInstantiation {
  def newInstance[R <: List, U <: AnyRef](cl: Class[U]): F[R] => F[R**Uninitialized[U]] = f => f.new_int(cl)
}
