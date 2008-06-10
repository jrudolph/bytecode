package net.virtualvoid.bytecode

trait Nil/*[ThisT<:Nil[ThisT]]*/{
  //val self:ThisT = this.asInstanceOf[ThisT]
  
  //def bipush(i:int):S[int,ThisT]
  //def op[Res](f:ThisT => Res):Res
}

trait Empty extends Nil/*[Empty]*/{
}

trait S[Top,Rest] extends Nil/*[S[Top,Rest]]*/{
  def pop:Rest
  def dup:S[Top,S[Top,Rest]]
}


trait Locals{
  
}
trait NoLocal extends Locals{}
trait Local[T,R<:Locals] extends Locals{}

trait Target[StackT,LT]{
  def jmp:(StackT,LT) => S2[StackT,LT]
}

trait Bottom
trait S2[+StackT<:Nil,+LT<:Locals] extends Bottom{
  def op[Res<:Nil](f:StackT => Res):S2[Res,LT]
  def lop[Res<:Bottom](f:(StackT,LT) => Res):Res
  //def op[LT2,LR,Res<:S2[LT2,LR]](f:S2[StackT,LT] => Res):Res
  
  //def bipush(i:int):S2[S[int,StackT],LT]
    
  //def target:Target[StackT,LT]
  
  def stack:StackT
}

object Test2{
  val a:S2[S[int,Empty],NoLocal] = null
  //val b:S2[Nil,Locals] = a
  //val x:Nothing = a
}

trait Empty2[LT<:Locals] extends S2[Empty,LT]{
  
}

trait Ops{
  def add[R]:S[int,S[int,R]] => S[int,R]
  def swap[Rest,A,B]:S[A,S[B,Rest]] => S[B,S[A,Rest]]
  def func[Rest,A,B](method:scala.reflect.Code[A=>B]): S[A,Rest] => S[B,Rest]
  def call[Rest,A,B](method:scala.reflect.Code[A=>Unit]): S[A,S[B,Rest]] => S[B,Rest]
  def pop[T,R]:S[T,R]=>R
  def dup[T,R]:S[T,R]=>S[T,S[T,R]]
  def checkcast[T,R,NewT<:T](newC:java.lang.Class[NewT]):S[T,R]=>S[NewT,R]
  def bipush[R](i:int):R=>S[int,R]
  
  def istore0[R,LR]:(S[int,R],Local[_,LR]) => S2[R,Local[int,LR]]
  def iload0[R,LR]:(R,Local[int,LR]) => S2[S[int,R],Local[int,LR]]
  
  def istore1[R,L0,LR]:(S[int,R],Local[L0,Local[_,LR]]) => S2[R,Local[L0,Local[int,LR]]]
  def iload1[R,L0,LR]:(R,Local[L0,Local[int,LR]]) => S2[S[int,R],Local[L0,Local[int,LR]]]
  
  def store0[T,R,LR]:(S[T,R],Local[_,LR]) => S2[R,Local[T,LR]]
  def load0[T,R,LR]:(R,Local[T,LR]) => S2[S[T,R],Local[T,LR]]
  
  def store1[T,R,L0,LR]:(S[T,R],Local[L0,Local[_,LR]]) => S2[R,Local[L0,Local[T,LR]]]
  def load1[T,R,L0,LR]:(R,Local[L0,Local[T,LR]]) => S2[S[T,R],Local[L0,Local[T,LR]]]
  
  def iinc0[S,LR]:(S,Local[int,LR]) => S2[S,Local[int,LR]]
  def iinc1[S,L0,LR]:(S,Local[L0,Local[int,LR]]) => S2[S,Local[L0,Local[int,LR]]]
  
  //def jmp[S<:Nil,L<:Locals](pos:S2[S,L]):(S,L)=>S2[S,L]
  def if_icmplt[R,LT,LR](pos:S2[R,Local[LT,LR]]=>S2[Nil,_]):(S[int,S[int,R]],Local[LT,LR]) => S2[R,Local[LT,LR]]
  
  def target[S<:Nil,L<:Locals](s:S2[S,L]):Target[S,L] = null
}

trait Mensch{}
trait Gustav extends Mensch{}
trait Gustavson extends Gustav{}

object Bytecode2{
  type FS[A] = Empty2[Local[A,NoLocal]]
  
  def test2 = {
    val start:S2[S[int,Empty],Local[int,Local[int,NoLocal]]] = null
    val ops:Ops=null
    import ops._
    
    //val jp = jmp(start);
    start
      .op(bipush(10))
      //.op(pop)
      .op(dup)
      .op(add)
      //.op(dup)
      .op(pop)
      .lop(target(start).jmp)
  }
  
  /*
   Severity and Description	Path	Resource	Location	Creation Time	Id
type mismatch;
 found   : (net.virtualvoid.bytecode.Empty, net.virtualvoid.bytecode.Local[_$6,net.virtualvoid.bytecode.Local[_$7,net.virtualvoid.bytecode.NoLocal] forSome { type _$7 }] forSome { type _$6 }) => Nothing
 required: (net.virtualvoid.bytecode.Empty) => ?	BytecodeDSL/src/net/virtualvoid/bytecode	Bytecode.scala	Unknown	1213102669296	22472

   */
  
  def test = {
    /* Simple loop test
   LINENUMBER 4 L0
    ICONST_0
    ISTORE 0: res
   L1
    LINENUMBER 5 L1
    ICONST_1
    ISTORE 1: i
   L2
    GOTO L3
   L4
    LINENUMBER 6 L4
    ILOAD 0: res
    ILOAD 1: i
    IADD
    ISTORE 0: res
   L5
    LINENUMBER 5 L5
    IINC 1: i 1
   L3
    ILOAD 1: i
    BIPUSH 10
    IF_ICMPLT L4
   L6
    LINENUMBER 7 L6
    ILOAD 0: res
    IRETURN
   L7    
     */
    val a:S2[S[int,Empty],NoLocal] = null
    //val b:S2[Nil,Locals] = a
    
    val s:Empty2[Local[_,Local[_,NoLocal]]] = null
    val ops:Ops=null
    import ops._
    
    val loopStart:S2[Empty,Local[int,Local[int,NoLocal]]] = s
      .op(bipush(0))
      .lop(store0)
      .op(bipush(1))
      .lop(store1)
      .lop(iinc1)
    
    type LI2 = Local[int,Local[int,NoLocal]]
    type SI2 = S[int,S[int,Empty]]
    
    //val j:(Empty,LI2)=>S2[Empty,LI2] = jmp(loopStart)   
    //val k = jmp(loopStart)
    
    loopStart
      .lop(load1)
      .op(bipush(10))
      .lop(if_icmplt(x=>{
                       //val y:S2[Empty,LI2] = 
                        x.lop(load0)
                         .lop(load1)
                         .op(add)
                         //.op(dup)
                         .lop(store0)
                         .lop(iinc1)
                         .lop(target(loopStart).jmp)
                       
                        null
                     }
      ))
      .lop(load0)
      .op(pop)// eigentlich return
    s
  }
  
  /*
   */
  
  def main(args:Array[String]):Unit = {
    val ops:Ops=null
    import ops._
    
    val f:Empty=>S[int,Empty] = null//func(_.bipush(5))
    
    val g:S[int,Empty] => S[Gustav,Empty] = null
    
    def wurst[R]:S[Gustav,R]=>S[String,R] = null
  
    val e:Empty2[Local[String,NoLocal]] = null
    val x:S[java.lang.String,Empty] = 
      e.op(bipush(5))
      .op(g)
      .lop(store0)
      .lop(load0)
      .op(wurst)
      .op(pop)
      .lop(load0)
      .op(wurst)
      .op(pop)
      .lop(load0)
      .op(func(_.toString))
      .op(pop)
      .lop(load0)
      .op(checkcast(classOf[Gustavson]))
      .op(func(_.toString))
      .stack
      //.op(pop)
    e
  }
}
/*
 Severity and Description	Path	Resource	Location	Creation Time	Id
type arguments [net.virtualvoid.bytecode.Empty,Nothing,net.virtualvoid.bytecode.Local[int,net.virtualvoid.bytecode.NoLocal]] do not conform to method iload0's type parameter bounds [R,LR,L <: net.virtualvoid.bytecode.Local[int,LR]]	BytecodeDSL/src/net/virtualvoid/bytecode	Bytecode.scala	Unknown	1213092657648	22262

 
 */

/*object Bytecode {
  def int2str(i:int):String = java.lang.Integer.toString(i)
  def println(i:int) = System.out.println(i)
  def main(args:Array[String]):Unit = {
    System.out println "Hallo Welt"
    val n:Empty = null
    val ops:Ops = null
    import ops._
    val s = 
      n.bipush(5).dup.op(add).dup
    s.dup.op(call(println(_)))
      .op(func(int2str(_)))
      .pop
      .bipush(12)
      .op(add)
      .bipush(5)
      .op(func(Integer.toString(_)))
      .op(call(System.out.println(_:String)))
      .dup
      .op(func(Integer.toString(_)))
      .op(swap)
      .dup
      .op(add)
    s
  }
}
*/