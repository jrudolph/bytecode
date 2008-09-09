package net.virtualvoid.bytecode.v2

object TypeTest {
  import Bytecode._
  import Bytecode.Implicits._

  trait papply[f2[_,_],x]{
    type f[y] = f2[x,y]
  }

  //type apply[f[_],x] = f[x]

  trait innerpapp[i[_,_],o[_],x]{
    type f[y] = o[i[y,x]]
  }

  trait Zippering[L<:List,Cur,R[_]]{
    def store[T](value:T):R[L**T]
  }

  type Id[x] = x
  def zipList[R<:List,T](l:R**T):Zippering[R,T,Id] = null
  def zipListOne[R<:List,T2,T1](l:R**T2**T1):Zippering[R,T2,papply[Cons,T1]#f] = null
  def zipZip[R<:List,T,Cur,Re[_]](z:Zippering[R**T,Cur,Re]):Zippering[R,T,innerpapp[Cons,Re,Cur]#f] = null

  //def apply[A[_,_],B,C](x:papply[A,B]#f[C]):A[B,C] = null.asInstanceOf[A[B,C]]

  def test[W[_]](x:Zippering[_,_,W]) = null

  def test1 = {
    val x:Nil**Int = null
    val y = zipList(x)
    val z:Nil**String = y.store("Wurst")

    val x2:Nil**Byte**String**Int = null
    //val y2:Zippering[Nil**String,Int,Id] = zipList(x2)
    val y3 = zipListOne(x2)
    //val z2:Nil**String**Float = y2.store(5f)
    //val z3:Nil**Float**Int = apply(y3.store(5f))

    val y4 = zipZip(y3)
    //val z4:Nil**Float**String**Int = y4.store("Wurst")
    //val a = zipZip(y2)
    y
/**
Severity and Description	Path	Resource	Location	Creation Time	Id
the kinds of the type arguments
(net.virtualvoid.bytecode.v2.Bytecode.Nil,Byte,String,net.virtualvoid.bytecode.v2.TypeTest.papply[net.virtualvoid.bytecode.v2.Bytecode.Cons,Int]#f[_])
do not conform to the expected kinds of the type parameters
(type R,type T,type Cur,type Re).
net.virtualvoid.bytecode.v2.TypeTest.papply[net.virtualvoid.bytecode.v2.Bytecode.Cons,Int]#f[_]'s
type parameters do not match type Re's expected parameters:
type f has one type parameter, but type Re has one

     */
  }
}