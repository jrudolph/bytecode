package net.virtualvoid.bytecode.v1

trait Nu
trait Nu0 extends Nu
object N0 extends Nu0
case class N[C<:Nu](inner:C) extends Nu

trait Descender[+X<:Nu,+Y<:St]

trait St
trait St0 extends St
object S0 extends St0
case class S[+T,+R<:St](top:T,rest:R) extends St{
}

trait Getter[T]{
  def get:T
}

object test{
  implicit def togetter[T,R<:St,ST<:S[T,R],D <% Descender[Nu0,ST]](desc:D):Getter[T] = null
  def descend[N<:Nu,ST<:St](st:ST,n:N):Descender[N,ST] = null
  implicit def autoDescend[NT<:Nu,ST<:St,T,ST2<:S[T,ST],NT2<:N[NT],D <: Descender[NT2,ST2]](d:D):Descender[NT,ST] = null

  def tester{
    val s = S(5,S("wurst",S(3,S0)))

    val x:Nu0 = null
    val N1:N[Nu0] = N(N0)
    val N2:N[N[Nu0]] = N(N1)

    val declDesc:Descender[Nu0,S[int,S[String,St0]]] = null
    val i:int = togetter(declDesc).get

    val desc:Descender[Nu0,S[int,S[String,S[int,St0]]]] = descend(s,N0)
    val i2:int = togetter(desc).get
    val i3:int = desc.get

    val desc2:Descender[N[Nu0],S[int,S[String,S[int,St0]]]] = descend(s,N1)
    val desc3:Descender[Nu0,S[String,S[int,St0]]] = autoDescend(desc2)
    val desc4:Descender[Nu0,S[String,S[int,St0]]] = desc2
    val str1:String = togetter(desc4).get
    val str2:String = desc4.get
    val str3:String = desc2.get
    val int12:int = desc4.get

    //val str:String = s.n(N0).get

    s
  }
}

/*
Severity and Description	Path	Resource	Location	Creation Time	Id
Severity and Description	Path	Resource	Location	Creation Time	Id
diverging implicit expansion for type
 *   (net.virtualvoid.bytecode.v1.Descender[net.virtualvoid.bytecode.v1.N[net.virtualvoid.bytecode.v1.Nu0],net.virtualvoid.bytecode.v1.S[int,net.virtualvoid.bytecode.v1.S[String,net.virtualvoid.bytecode.v1.S[int,net.virtualvoid.bytecode.v1.St0]]]])
 * => net.virtualvoid.bytecode.v1.Descender[net.virtualvoid.bytecode.v1.N[NT],net.virtualvoid.bytecode.v1.S[T,ST]]
starting with method autoDescend in object test	BytecodeDSL/src/net/virtualvoid/bytecode/v1	NewNumberTest.scala	Unknown	1220522666996	39199

 */