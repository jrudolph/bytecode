package net.virtualvoid.bytecode.v2

object BugTest {
  trait papply[f2[_,_],x]{
    type f[y] = f2[x,y]
  }

  trait X[A,B]
  trait Y[A[_]]{
    def set[T](t:T):A[T]
  }

  def resolve[A[_,_],B,C](x:papply[A,B]#f[C]):A[B,C] = null.asInstanceOf[A[B,C]]

  val b:Y[papply[X,Int]#f] = null
  val c = b.set(5f)
  //val d:X[Int,Float] = resolve(c)
}
