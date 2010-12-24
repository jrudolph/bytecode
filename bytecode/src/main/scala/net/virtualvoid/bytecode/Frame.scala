package net.virtualvoid.bytecode

trait F[+ST<:List] extends backend.BackendSupport[ST] {
  def ~[X](f:F[ST]=>X):X = f(this)
}

trait Target[ST<:List]{
  def jmp:F[ST] => Nothing
}

trait ROLocal[+T] {
  def load[ST<:List, T2 >: T]:F[ST] => F[ST**T2]
}
trait Local[T] extends ROLocal[T] {
  def store[ST<:List]:F[ST**T] => F[ST]
}

trait MethodHandle {
  import java.lang.reflect.Method
  
  def method: Method
  def numParams: Int
  protected def normalCall[X <: List, R <: List, U]: F[X] => F[R**U]
  protected def   unitCall[X <: List, Y <: List   ]: F[X] => F[Y] 
}
