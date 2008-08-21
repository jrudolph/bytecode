package net.virtualvoid.bytecode.v1

trait Stack{}
trait Empty extends Stack{}
trait S[T,R<:Stack] extends Stack{}

trait Local{}
trait NoLocal extends Local{}
trait L[T,R<:Local]{}

/* A Frame state, consisting of stack and locals*/
trait F[StackT<:Stack,LocalT<:Local]{
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

trait Int2Stack[R<:Stack,L<:Local]{
  def iadd():F[S[int,R],L]
}
trait NotEmptyStack[R,L<:Local]{
  def pop():F[R,L]
}

object Simulation{
  implicit def int2Stack[R,LT](st:F[S[int,S[int,R]],LT]):Int2Stack[R,LT] = null
  implicit def notEmptyStack[T,R,LT](s:F[S[T,R],LT]):NotEmptyStack[R,LT] = null
  
  type E[LT<:Local] = F[Empty,LT]
  
  def test = {
    val ops:Ops = null
    val x:F[Empty,NoLocal] = null
    import ops._    
    x.op(bipush(12))
     .op(dup)
     .iadd
     .pop
     .op(bipush(5))
     .sop(pops)
    x
  }
}
