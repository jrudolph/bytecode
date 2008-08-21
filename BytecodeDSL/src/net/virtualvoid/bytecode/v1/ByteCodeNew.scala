package net.virtualvoid.bytecode.v1

trait Stack{}
trait Empty extends Stack{}
trait S[T,R<:Stack] extends Stack{}

trait Local{}
trait NoLocal extends Local{}
trait L[T,R<:Local] extends Local{}

/* A Frame state, consisting of stack and locals*/
trait F[+StackT<:Stack,+LocalT<:Local]{
  
  /*
  /* Apply a transformation to this frame state
   * which changes both stack and local types
   */
  def op[Res](func:F[StackT,LocalT]=>Res):Res  
  /* Apply a transformation to this frame state
   * which only affects the stack
   */
  def sop[Res<:Stack](f:StackT => Res):F[Res,LocalT]
  /* Apply a transformation which only affects the locals
   */
  def lop[Res<:Local](f:LocalT => Res):F[StackT,Res]
  
  */
}

trait Ops{
  type T[FS,TS,FL,TL] = F[FS,FL] => F[TS,TL]
  type ST[FS,TS,L] = T[FS,TS,L,L]
  
  def sop[L,From,To](func: From => To):ST[From,To,L]
  def lop[X<:Stack,From<:Local,To<:Local](func:From => To):T[X,X,From,To]
  
  def iadds[R]:S[int,S[int,R]] => S[int,R]
  def iadd[LT,R<:Stack]:ST[S[int,S[int,R]],S[int,R],LT] = sop(iadds)
  
  def dups[T,R]:S[T,R] => S[T,S[T,R]]
  def dup[T,R<:Stack,L]:ST[S[T,R],S[T,S[T,R]],L] = sop(dups)
  
  def bipushs[R](i:int): R => S[int,R]
  def bipush[L,R<:Stack](i:int):ST[R,S[int,R],L] = sop(bipushs(i))
  
  def pops[T,R]:S[T,R]=>R
  def pop[T,R<:Stack,L]:ST[S[T,R],R,L] = sop(pops)
}



/*object Simulation{
  type E[LT<:Local] = F[Empty,LT]
  
  def test = {
    val ops:Ops = null
    val x:F[Empty,NoLocal] = null
    import ops._
    val v:Implicits[_] = null//new Implicits(){}
    import v._
    x.op(bipush(12))
     .op(dup)
     .iadd
     .pop
     .op(bipush(5))
     .sop(pops)
    x
  }
}*/

trait ByteletCompiler{
  /*
   * compiles some bytecode directly into an invocable Function1
   */
  def compile[T,U](code : F[Empty,L[T,NoLocal]] => F[S[U,Empty],Local]): T => U
}

abstract class AbstractFunction1 extends Function1[AnyRef,AnyRef]{}

/*object Compiler extends ByteletCompiler{
  def compile[T,U](code : F[Empty,L[T,NoLocal]] => F[S[U,Empty],Local]): T => U = {
    
  }
 * 
Severity and Description	Path	Resource	Location	Creation Time	Id
type arguments [SI] do not conform to trait Backend's type parameter bounds [V[_,_ <: net.virtualvoid.bytecode.v1.Stack] <: net.virtualvoid.bytecode.v1.S[_$1,_$2] forSome { type _$1; type _$2 }]	BytecodeDSL/src/net/virtualvoid/bytecode/v1	ByteCodeNew.scala	Unknown	1219320901589	28668

}*/

trait Implementation{
  type BS <: Stack
  type ST[_,_] <: S[_,_] with BS
  
  trait Implicits[FT[_<:BS,_<:Local]<:F[_,_]]{
    trait WithStackOps[R<:Stack,L<:Local]{
      def ldc(str:String):FT[ST[String,R],L]
    }  
    trait Int2Stack[R<:Stack,L<:Local] extends WithStackOps[R,L]{
      def iadd():FT[ST[int,R],L]
    }
    trait NotEmptyStack[R<:Stack,L<:Local] extends WithStackOps[R,L]{
      def pop():FT[R,L]
    }
    
    implicit def int2Stack[R<:Stack,LT<:Local](st:FT[S[int,S[int,R]],LT]):Int2Stack[R,LT]
    implicit def notEmptyStack[T,R<:Stack,LT<:Local](s:FT[S[T,R],LT]):NotEmptyStack[R,LT] = null
    implicit def opsStack[R<:Stack,L<:Local](s:FT[R,L]):WithStackOps[R,L]
  }
  
  trait Backend {
    def iadd[R<:Stack](s:ST[int,ST[int,R]]):ST[int,R]
    def ldc[R<:Stack](s:R,str:String):ST[String,R]
  }

  class SimpleFrame[ST2<:BS,LT<:Local](val st:ST2,val lo:LT) extends F[ST2,LT]
  class BackedFrameImplicit(backend:Backend) extends Implicits[SimpleFrame]{  
    def opsStack[R<:Stack,L<:Local](f:SimpleFrame[R,L]):WithStackOps[R,L] = new WithStackOps[R,L]{
      def ldc(str:String):SimpleFrame[ST[String,R],L] = 
        new SimpleFrame(backend.ldc(f.st,str),f.lo) 
    }
    def int2Stack[R<:Stack,LT<:Local](f:SimpleFrame[ST[int,ST[int,R]],LT]):Int2Stack[R,LT] = new Int2Stack[R,LT]{
      def iadd():SimpleFrame[ST[int,R],LT] =
        new SimpleFrame(backend.iadd(f.st),f.lo)
    }
  }
}

object BackendImpl extends Implementation{
  class SimpleStack[T,R<:Stack](val top:T,val rest:R) extends S[T,R]
  
  object InterpretingBackend extends Backend {
    def iadd[R<:Stack](s:SimpleStack[int,SimpleStack[int,R]]) = new SimpleStack(s.top + s.rest.top,s.rest.rest)
    def ldc[R<:Stack](s:R,str:String) = new SimpleStack(str,s)
  }    
}

/* 
 * Severity and Description	Path	Resource	Location	Creation Time	Id
the kinds of the type arguments (net.virtualvoid.bytecode.v1.F) do not conform to the expected kinds of the type parameters (type FT) in trait Implicits.
net.virtualvoid.bytecode.v1.F's type parameters do not match type FT's expected parameters: type StackT's bounds >: Nothing <: net.virtualvoid.bytecode.v1.Stack are stricter than type _'s declared bounds >: Nothing <: Any, type LocalT's bounds >: Nothing <: net.virtualvoid.bytecode.v1.Local are stricter than type _'s declared bounds >: Nothing <: Any	BytecodeDSL/src/net/virtualvoid/bytecode/v1	ByteCodeNew.scala	Unknown	1219321746406	28739

*/
class BackedFrame[+StackT<:Stack,+LocalT<:Local] extends F[StackT,LocalT]{
   
  def op[Res](func:F[StackT,LocalT]=>Res):Res = null.asInstanceOf[Res]
  def sop[Res<:Stack](f:StackT => Res):F[Res,LocalT] = null
  def lop[Res<:Local](f:LocalT => Res):F[StackT,Res] = null
}

object Wurst{
  def main(args:Array[String]):Unit ={
    val a = {x:int => x+6}
    
    val c:ByteletCompiler = null
    val x = new BackendImpl.BackedFrameImplicit(null)
    import x._
    c.compile[Integer,String](x.opsStack(_).ldc("test"))
    
    System.out.println(new Function1Creator()(4));
  }
}