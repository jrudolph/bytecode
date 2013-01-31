package net.virtualvoid.bytecode
package backend

/** The main entry point into bytecode generation
 *
 *  Every backend has to implement these methods.
 */
trait ByteletCompiler {

  def compile[T1 <: AnyRef, R <: AnyRef](par1Cl: Class[T1], retCl: Class[R])
                                        (code: Local[T1] => Return[R] => F[Nil] => Nothing)
      : T1 => R	  
  def compile[T1 <: AnyRef, T2 <: AnyRef, R <: AnyRef](cl1:Class[T1], cl2:Class[T2], retCl:Class[R])
                                                      (code: (Local[T1],Local[T2]) => Return[R] => F[Nil] => Nothing)
      : (T1, T2) => R


  /** A shortcut which infers the runtime return Class from the parameters */
  def compile[T1 <: AnyRef, R <: AnyRef](cl: Class[T1])
                                        (code: Local[T1] => F[Nil] => F[Nil**R])
                                        (implicit mf: scala.reflect.Manifest[R])
      : T1 => R = 
    compile(cl, mf.erasure.asInstanceOf[Class[R]])(p1 => ret => f => f ~ code(p1) ~ ret.jmp)

  /** A shortcut which uses just manifests for types */
  def apply[T1 <: AnyRef, R <: AnyRef](code: Local[T1] => F[Nil] => F[Nil**R])
                                      (implicit mfT1: scala.reflect.Manifest[R], mfR: scala.reflect.Manifest[R])
      : T1 => R =
    compile[T1, R](mfT1.erasure.asInstanceOf[Class[T1]], mfR.erasure.asInstanceOf[Class[R]]) {
      p1 => ret => f => f ~ code(p1) ~ ret.jmp
    }
}

/** A token which is used to return a value from the function */
trait Return[T] {
  def jmp: F[Nil**T] => Nothing
}
