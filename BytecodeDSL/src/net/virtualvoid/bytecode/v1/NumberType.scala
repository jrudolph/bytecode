package net.virtualvoid.bytecode.v1

trait Number
trait NoNumber <: Number
trait N[C <: Number] <:Number

object types{
  /*type N0 = NoNumber
  type N1 = N[NoNumber]
  type N2 = N[N[NoNumber]]*/
  
  val N0:NoNumber = null
  val N1:N[NoNumber] = null
  val N2:N[N[NoNumber]] = null
}

/*trait Blubber[L<:Local,X<:Number,Y]{
  def blub(x:X):Y
}*/

trait Descender[L<:Local,X<:Number]{}
trait Descended[T]{
  def load:T
}

trait Descendable[L<:Local]{
  def v[N<:Number](n:N):Descender[L,N] = null
}
trait NumberDescender[L<:Local,X<:Number]{}

object test{
  import types._
  implicit def descending[T,R,NR](d:Descender[L[T,R],N[NR]]):Descender[R,NR] = null
  implicit def descending0[L,NR](d:Descender[L,N[NR]]):NumberDescender[L,NR] = null
  implicit def descending1[T,R,N](d:NumberDescender[L[T,R],N]):Descender[R,N] = null
  implicit def descended[T,R](d:Descender[L[T,R],NoNumber]):Descended[T] = null
  implicit def descendable[L](l:L):Descendable[L] = null
  
  //implicit def descend1[A,B,T](x:Descender[L[A,L[T,B]],N[NoNumber]]):Descended[T] = null
  //implicit def descend2[L0,L1,L2,R,T](x:Descender[L[L0,L[L1,L[L2,R]]],N[N[NoNumber]]]):Descended[L2] = null
  def test {
    val a:L[int,L[String,L[Double,NoLocal]]] = null
    val x0:int = descended(a.v(N0)).load
    val x1:String = descended(descending(descendable(a).v(N1))).load
    val x2:String = descending(descendable(a).v(N1)).load
    val x3:String = descending(a.v(N1)).load
    val x4:Double = descending(descending(a.v(N2))).load
    
    val x5:String = descending1(descending0(a.v(N1))).load
    val x6:String = descending0(a.v(N1)).load
    val x7:String = descending1(a.v(N1)).load
    
    //val x8:String = descend1(a.v(N1)).load
    //val x8b:Double= descend2(a.v(N2)).load
    val x9:int = a.v(N0).load
    val x10:String = a.v(N1).load
    val x11:Double = a.v(N2).load
    ()
  }
}

/*
 
 Severity and Description	Path	Resource	Location	Creation Time	Id
inferred type arguments [net.virtualvoid.bytecode.v1.L[int,net.virtualvoid.bytecode.v1.L[String,net.virtualvoid.bytecode.v1.NoLocal]]] do not conform to method descendable's type parameter bounds [L <: net.virtualvoid.bytecode.v1.Local]	BytecodeDSL/src/net/virtualvoid/bytecode/v1	NumberType.scala	Unknown	1217423563029	25002


 */