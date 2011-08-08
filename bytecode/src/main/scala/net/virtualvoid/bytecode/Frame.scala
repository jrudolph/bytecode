package net.virtualvoid.bytecode

import scala.annotation.unchecked.uncheckedVariance

trait F[+ST<:List] extends backend.BackendSupport[ST] {
  def ~[X](f:F[ST]=>X):X = f(this)
}

trait Target[ST<:List]{
  def jmp:F[ST] => Nothing
}

/**
 * A capability representing a readable local variable slot.
 */
trait LocalR[+T] {
  def load[ST<:List, T2 >: T]: F[ST] => F[ST ** T2]
}

/**
 * The writable extension of LocalR. As usual, with writability covariance is
 * lost. If a local variable slot is passed into a function and the local variable
 * should only be read inside the function, use LocalR instead of Local because
 * then covariance works like a client would expect it (e.g. for a parameter of type
 * LocalR[java.lang.Iterable[AnyRef]] you can pass a Local[java.util.List[AnyRef]]
 * which is often the right thing.
 */
trait Local[T] extends LocalR[T] {
  def store[ST<:List]:F[ST**T] => F[ST]
}

trait MethodHandle {
  import java.lang.reflect.Method
  
  def method: Method
  def numParams: Int
  protected def normalCall[X <: List, R <: List, U]: F[X] => F[R**U]
  protected def   unitCall[X <: List, Y <: List   ]: F[X] => F[Y] 
}
