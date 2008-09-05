package net.virtualvoid.bytecode.v2

object Test2{
  //import Bytecode._
  trait List
  trait Nil extends List
  trait Cons[R<:List,T] extends List
  trait Zip[R,Cur,B<:List]

  type **[R<:List,T] = Cons[R,T]

  implicit def toList[R<:List,Cur](z:Zip[R,Cur,Nil]):Cons[R,Cur] = null
  implicit def desc[R,Cur,BR<:List,BT](z:Zip[R,Cur,Cons[BR,BT]]):Zip[Cons[R,Cur],BT,BR] = null

  trait Builder[R[_]]{
    def build[T](t:T):R[T]
  }

  type a[x,y] = Cons[x,y]
  type t[x] = a[_,x]

  val test:Builder[Cons[_,X] forSome {type X <: List}] = null
  val u:Cons[Nil,Int] = test.build[Int](4)

  val a:Zip[Nil,Int,Cons[Cons[Nil,String],Float]] = null
  val b:Nil**Int**Float**String = toList(a)
}